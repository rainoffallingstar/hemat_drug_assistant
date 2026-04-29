# mellon 迁移总结：R Shiny -> Python + React + Go

> 日期：2026-04-28  
> 项目：血液科用药助手 (Hematological Drug Assistant)  
> 当前正式访问地址：`https://rainoffallingstar.shinyapps.io/hemat_drug_assistant/`

## 一、最终结论

这次 shinyapps.io 上的 `404 page not found`，最终确认不是单一故障点，而是三层路径机制叠加导致的：

1. 前端静态资源使用了以 `/` 开头的绝对路径，浏览器会去请求宿主根路径 `/assets/*`。
2. 前端 API 调用使用了 `fetch('/api/...')`，请求会落到宿主根路径 `/api/*`，而不是应用子路径下。
3. shinyapps.io 在转发 Python Shiny 应用时，会给请求附加应用前缀和会话前缀；代理层如果不先剥掉这些前缀，就会把错误路径继续转发给 Go，Go 自然返回 404。

所以，真正解决 404 的关键不是把架构切成 `static_assets`，而是让整个链路在 shinyapps.io 子路径模式下保持“相对路径一致”，并且在 Python 代理转发前做路径归一化。

## 二、当前线上架构

```text
浏览器
  -> /hemat_drug_assistant/
  -> Python Shiny 入口 (app.py)
  -> Go API / static server (bin/mellon-api)
  -> www/ React 构建产物 + data/ JSON 数据
```

角色分工如下：

- `ui/`: React 18 + TypeScript + Vite 前端源码
- `www/`: React 构建产物，最终由 Go 静态服务输出
- `cmd/` + `internal/`: Go API、评分逻辑、方案计算逻辑
- `app.py`: shinyapps.io 容器入口，负责拉起 Go，并将 HTTP 请求代理给 Go
- `scripts/deploy_shinyapps.sh`: 轻量部署脚本，只打包运行时需要的文件

这意味着当前 shinyapps.io 并不是“直接运行 React”，也不是“退回纯 Python 计算”，而是仍然保留 `Python 入口 + Go 实际服务` 这条链路。

## 三、404 的根因拆解

### 3.1 绝对静态资源路径为什么会 404

应用部署后，正式入口不是网站根路径 `/`，而是：

```text
/hemat_drug_assistant/
```

如果 `index.html` 中引用的是：

```html
<script src="/assets/index.js"></script>
```

浏览器会直接请求：

```text
https://rainoffallingstar.shinyapps.io/assets/index.js
```

但真实资源并不在站点根路径，而在应用子路径下，所以会得到 404。

这就是为什么 Vite 的 `base: '/'` 在 shinyapps.io 上不合适。它只适用于应用挂在域名根路径时的场景。

### 3.2 绝对 API 路径为什么会 404

同理，如果前端代码写的是：

```ts
fetch('/api/health')
```

浏览器会请求：

```text
https://rainoffallingstar.shinyapps.io/api/health
```

而不是：

```text
https://rainoffallingstar.shinyapps.io/hemat_drug_assistant/api/health
```

所以即便 Go 服务器和代理本身是正常的，前端请求也会直接打偏到站点根路径，表现成 API 404。

### 3.3 为什么仅修正前端路径还不够

shinyapps.io 对 Python Shiny 应用还有一层自己的路径包装。

在真实运行时，请求路径不只是简单的：

```text
/hemat_drug_assistant/api/health
```

还可能带有会话前缀，例如：

```text
/_w_<session_id>/hemat_drug_assistant/api/health
```

或者把这些信息放进 ASGI 的 `root_path` 中。

如果代理层直接把 `scope["path"]` 原封不动转发给 Go，那么 Go 收到的可能是：

```text
/hemat_drug_assistant/api/health
```

甚至：

```text
/_w_<session_id>/hemat_drug_assistant/api/health
```

而 Go 端真正实现的路由是：

```text
/api/health
/assets/*
/
```

也就是说，Go 本身并不知道 shinyapps.io 的应用前缀和会话前缀，所以这一步如果不做路径清洗，后端仍然会继续 404。

### 3.4 shinyapps.io 注入机制带来的额外现象

这次排障里还有一个很重要的观察：shinyapps.io 会给 Python Shiny 响应注入自己的运行时脚本，并可能改写页面基础路径。

这解释了两个现象：

1. 为什么过去很多“直接改前端 HTML”的尝试并不稳定。
2. 为什么浏览器里还能看到由 shinyapps.io 注入脚本产生的控制台报错，例如 `shiny-server-client.min.js` 里的 `TypeError: $ is not a function`。

这个残余报错目前不影响 React 正常渲染，也不影响 `/api/*` 请求返回 200；它属于平台注入脚本与我们 SPA 页面并存时的噪音，而不是本轮 404 的主因。

## 四、实际修复方案

### 4.1 前端构建改为相对静态资源路径

文件：`ui/vite.config.ts`

把：

```ts
base: '/'
```

改成：

```ts
base: './'
```

效果是构建后的 `www/index.html` 会引用：

```html
./assets/...
```

而不是：

```html
/assets/...
```

这样浏览器会以当前应用路径为基准解析资源地址，最终落到：

```text
/hemat_drug_assistant/assets/*
```

这一步解决的是“前端资源一打开就 404”的问题。

### 4.2 前端 API 改为相对路径调用

文件：`ui/src/api/client.ts`

把：

```ts
fetch('/api/...')
```

改成：

```ts
fetch('api/...')
```

这样浏览器会按当前页面地址解析请求，把 API 请求发送到：

```text
/hemat_drug_assistant/api/*
```

而不是宿主根路径 `/api/*`。

这一步解决的是“页面能打开，但所有接口 404”的问题。

### 4.3 代理层在转发前剥离 shinyapps.io 前缀

文件：`app.py`

本轮最关键的后端修复在这里。

新增了两部分核心逻辑：

1. `normalized_app_prefix()`
2. `upstream_path_for_scope()`

它们的职责是：

- 优先读取 ASGI `scope` 里的 `root_path`
- 再结合可选的 `MELLON_URL_PREFIX`
- 在转发到 Go 前，把 `/hemat_drug_assistant` 以及 `/_w_<session>/...` 这类平台前缀剥掉
- 最终只把 Go 真正理解的路径转发出去，例如 `/api/health` 或 `/assets/index.js`

这一步解决的是“浏览器路径已经对了，但代理仍然把错误路径交给 Go”的问题。

### 4.4 启动探针和失败兜底

同样在 `app.py` 中，还补了可观测性和失败提示：

- 启动时记录 Go 二进制预期路径
- 拉起 Go 后轮询 `/api/health`
- 记录前若干个代理请求探针日志
- 当 Go 不可用时，返回明确的 `503` 文本，而不是让前端表面上看起来像随机 404

这一步不直接修复 404，但它让我们第一次能够明确区分：

- 是平台根本没走到 `app.starlette_app`
- 还是代理层已执行，但上游路径错了
- 还是 Go 进程没有成功启动

## 五、为什么之前会误以为必须改成 static_assets

之前得出“只能走 `static_assets`”这个判断，是因为在旧实验路径里，我们确实遇到了下面这些现象：

- Python Shiny 中间件经常看起来不生效
- 页面会被 Shiny 注入脚本污染
- 代理前缀处理不正确时，所有请求都像是“凭空 404”

在那种表象下，最容易得出的结论就是：Shiny 完全不可控，只能绕开。

但这次排障把链路拆开以后，发现实际情况更精细：

- 平台确实会注入脚本
- 但 `app.starlette_app` 代理层仍然是可执行的
- 真正致命的是路径前缀和相对路径问题
- 只要浏览器端和代理端都按“子路径部署”来处理，Go 方案仍然能在 shinyapps.io 上跑通

所以，`static_assets` 不是唯一方案，只是另一条更“平台原生”的路线；而当前项目已经证明，保留 Go 代理入口也可以稳定工作。

## 六、部署与默认目标的收口

### 6.1 正式应用

当前正式 shinyapps.io 应用为：

```text
title: hemat_drug_assistant
app id: 17244883
url: https://rainoffallingstar.shinyapps.io/hemat_drug_assistant/
```

### 6.2 部署脚本收口

`scripts/deploy_shinyapps.sh` 已调整为：

- 默认 `RSCONNECT_TITLE=hemat_drug_assistant`
- 默认从 `rsconnect-python/hemat_drug_assistant.json` 解析 `app_id`
- 继续只打包运行时所需文件，避免把 `ui/node_modules` 之类的大目录带上去

默认部署命令现在就是：

```bash
bash scripts/deploy_shinyapps.sh
```

如需覆盖目标应用，仍可显式指定：

```bash
RSCONNECT_TITLE=mellon bash scripts/deploy_shinyapps.sh
RSCONNECT_APP_ID=17244883 bash scripts/deploy_shinyapps.sh
RSCONNECT_NEW=1 RSCONNECT_TITLE=my-test-app bash scripts/deploy_shinyapps.sh
```

### 6.3 账号清理

本轮排障期间额外创建的测试应用已清理：

- `hemat_drug_assistant1` 已 purge
- `mellon-shinyapps` 已 purge

当前账号保留的相关应用中，正式入口是 `hemat_drug_assistant`；`mellon` 仍保留，作为历史部署，不在这轮中删除。

## 七、验证结果

本轮修复后的关键验证点如下：

1. 正式首页 `https://rainoffallingstar.shinyapps.io/hemat_drug_assistant/` 返回 `200`
2. 健康检查 `https://rainoffallingstar.shinyapps.io/hemat_drug_assistant/api/health` 返回 `200`
3. 当前 `www/index.html` 已输出 `./assets/...`，不再引用 `/assets/...`
4. 前端 API 已使用 `fetch('api/...')`，不再从站点根路径请求 `/api/...`
5. 线上页面可以正常渲染，核心 API 可用

仍然存在的残余现象：

- 浏览器控制台中可能看到 shinyapps.io 注入脚本的非阻塞报错：`TypeError: $ is not a function`
- 这不是本轮 404 的根因，也不影响当前核心业务功能

## 八、迁移过程中得到的原则

1. shinyapps.io 应用默认要按“子路径部署”来设计，不能把根路径部署当成默认前提。
2. 浏览器路径修正和代理路径修正必须同时做，只修一边通常不够。
3. 对这类平台问题，日志探针和失败兜底比盲改架构更重要；没有可观测性时，很容易把“路径错了”误判成“平台完全不支持”。
4. `static_assets` 仍然是值得保留的备选路线，但在当前项目里，它不是修复 404 的必要条件。

## 九、当前未解决项

1. shinyapps.io 注入的客户端脚本仍可能在控制台产生噪音报错。
2. `mellon` 旧应用还保留在线上，如果未来不再需要，可以另开一次运维清理。
3. `app.py` 目前承担了代理、探针、兜底三种职责，后续可以继续整理结构，但这不影响当前部署可用性。
