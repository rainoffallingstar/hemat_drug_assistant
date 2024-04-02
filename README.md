# hemat_drug_assistant

[![DOI](https://zenodo.org/badge/666609619.svg)](https://zenodo.org/badge/latestdoi/666609619)

<img src="https://github.com/rainoffallingstar/hemat_drug_assistant/blob/master/image/logo.jpg" height="200" align="right"/>

#### 介绍 (Generated by KIMI)

血液科用药助手是一个为血液学专业人员打造的综合性应用程序，旨在提供精准的药物剂量计算和治疗方案推荐。

应用程序功能亮点：

- 个性化药物剂量计算：根据患者的基本信息（如年龄、性别、体重、身高等），应用自动计算出适合患者的药物剂量，确保治疗方案的安全性和有效性。

- 治疗方案推荐：根据患者的疾病类型（如白血病、淋巴瘤等），提供针对性的治疗方案。

- 药物副作用与相互作用：详细列出各种药物的副作用和可能的药物相互作用，帮助在制定治疗方案时充分考虑到患者的安全和舒适。

- 预后评分系统和临床常用工具：包含多种血液病的预后评分系统，如IPI、MDAPSS等，帮助您更好地评估患者的治疗风险和效果。

- 方案切换与语言选择：应用程序支持在不同治疗方案之间轻松切换，并提供中英文界面，满足不同用户的需求。

- 实时更新与持续改进：我们将持续关注最新的血液科药物研究和治疗方案进展，及时更新应用程序，为您提供最前沿的治疗建议。

请注意，本应用程序正在积极开发中，我们建议您在将其纳入日常临床操作和决策过程之前仔细检查方案内容。

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
