# hemat_drug_assistant

#### 介绍

A shiny application developed for drug does calculation in clinical works.

#### 软件架构

1.  方案按方案名命名的单个excel文件保存在data文件夹下按疾病命名的子文件夹内

2.  pickerInput内的变量名由程序自动扫描清洗形成，本程序代码与数据文件夹解耦。

3.  excel文件的模板见temple.xlsx文件，具体内容如下：

    |     |       |     |                               |     |        |     |        |     |        |     |      |     |
    |-----|----------------|-----|-----|-----|-----|-----|-----|-----|-----|-----|-----|-----|
    |     | 药名  |     | 类型                          |     | 推荐值 |     | 最小值 |     | 最大值 |     | 单位 |     |
    |     |       |     |                               |     |        |     |        |     |        |     |      |     |
    |     | drug1 |     | chemo                         |     | 750    |     | 0      |     | 0      |     | mg   |     |
    |     |       |     |                               |     |        |     |        |     |        |     |      |     |
    |     | drug2 |     | monoclone（单抗类，固定计量） |     | 50     |     | 0      |     | 0      |     | mg   |     |
    |     |       |     |                               |     |        |     |        |     |        |     |      |     |
    |     | drug3 |     | chemo                         |     | 1.4    |     | 0      |     | 0      |     | mg   |     |
    |     |       |     |                               |     |        |     |        |     |        |     |      |     |
    |     | drug4 |     | chemo                         |     | 60     |     | 0      |     | 0      |     | mg   |     |
    |     |       |     |                               |     |        |     |        |     |        |     |      |     |
    |     | drug5 |     | mono（单抗类，计算计量）      |     | 375    |     | 0      |     | 0      |     | mg   |     |
    |     |       |     |                               |     |        |     |        |     |        |     |      |     |

#### 使用说明

1.  在rstudio内本地部署使用
2.  我们有一个部署实例，在[rsconnect.](https://rainoffallingstar.shinyapps.io/hemat_drug_assistant/)

#### 参与贡献

1.  Fork 本仓库
2.  新建 Feat_xxx 分支
3.  提交代码
4.  新建 Pull Request

#### 
