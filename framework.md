# METEOR包框架：全面集成的AMR元分析与分析工具

## 包设计概览

```
meteor/
├── R/                      # R源代码文件夹
│   ├── core/               # 核心功能
│   ├── data/               # 数据处理功能
│   ├── analysis/           # 分析功能
│   ├── visualization/      # 可视化功能 
│   ├── shiny/              # Shiny应用接口
│   ├── prediction/         # 预测与模拟功能
│   ├── integration/        # 外部数据集成功能
│   ├── reporting/          # 报告生成功能
│   └── utils/              # 工具函数
├── data/                   # 包含的数据集
├── inst/                   # 安装文件
│   ├── shiny-apps/         # Shiny应用程序
│   └── templates/          # 报告模板
├── vignettes/              # 使用指南
├── tests/                  # 测试文件
└── docs/                   # 文档
```

## 功能模块设计

### 1. 核心模块 (core)

**目的**: 提供包的基础功能和主要工作流程

```r
# 主要函数
export(meteor_version)              # 返回版本信息
export(initialize_meteor_project)   # 初始化项目
export(set_meteor_options)          # 设置全局选项
export(get_meteor_options)          # 获取当前选项
export(meteor_help)                 # 显示帮助信息
export(check_dependencies)          # 检查依赖包
export(launch_meteor)               # 启动主Shiny应用
```

### 2. 数据处理模块 (data)

**目的**: 导入、处理、验证和管理AMR数据

```r
# 数据导入函数
export(import_amr_data)             # 导入AMR数据(CSV, Excel, RData)
export(import_local_data)           # 导入研究者本地数据
export(connect_to_database)         # 连接到外部数据库
export(import_from_paper)           # 从论文提取数据辅助工具

# 数据验证与清洗
export(validate_data)               # 验证数据完整性和结构
export(check_data_quality)          # 检查数据质量问题
export(standardize_amr_data)        # 标准化数据字段
export(impute_missing_data)         # 处理缺失数据

# 数据整合与转换
export(merge_domains)               # 合并不同领域的数据
export(transform_data_format)       # 转换数据格式
export(aggregate_studies)           # 聚合多项研究数据
export(filter_amr_data)             # 按条件筛选数据

# 元数据管理
export(extract_metadata)            # 提取研究元数据
export(assess_study_quality)        # 评估研究质量
export(assign_quality_scores)       # 分配质量评分
export(summarize_dataset)           # 生成数据集概览
```

### 3. 分析模块 (analysis)

**目的**: 提供元分析和统计分析功能

```r
# 元分析核心函数
export(calculate_pooled_rate)       # 计算汇总耐药率
export(analyze_heterogeneity)       # 分析研究间异质性
export(perform_subgroup_analysis)   # 执行亚组分析
export(perform_sensitivity_analysis) # 执行敏感性分析
export(assess_publication_bias)     # 评估发表偏倚

# 高级分析
export(perform_meta_regression)     # 执行元回归分析
export(network_meta_analysis)       # 网络元分析
export(detect_outliers)             # 检测异常值
export(analyze_time_trends)         # 分析时间趋势
export(compare_domains)             # 比较不同领域数据

# 模式分析
export(detect_resistance_clusters)  # 检测耐药模式集群
export(identify_unusual_patterns)   # 识别异常模式
export(discover_correlated_resistance) # 发现相关耐药性
export(analyze_resistance_evolution) # 分析耐药性演变

# 本地数据分析
export(compare_with_meta)           # 将本地数据与元分析比较
export(calculate_deviation_statistics) # 计算偏离统计量
export(simulate_inclusion)          # 模拟将本地研究纳入元分析
export(calculate_influence)         # 计算本地研究影响
```

### 4. 可视化模块 (visualization)

**目的**: 创建静态和交互式数据可视化

```r
# 森林图系列
export(create_forest_plot)          # 创建基本森林图
export(create_interactive_forest)   # 创建交互式森林图
export(create_subgroup_forest)      # 创建分组森林图
export(create_cumulative_forest)    # 创建累积森林图

# 地理可视化
export(create_geo_map)              # 创建地理分布图
export(create_choropleth)           # 创建填色地图
export(create_region_comparison)    # 创建区域比较图
export(visualize_hotspots)          # 可视化热点区域

# 热图与模式可视化
export(create_resistance_heatmap)   # 创建耐药模式热图
export(create_trend_heatmap)        # 创建趋势热图
export(create_correlation_heatmap)  # 创建相关性热图
export(create_matrix_plot)          # 创建矩阵图

# One Health整合可视化
export(create_domain_comparison)    # 创建领域比较图
export(create_sankey_diagram)       # 创建桑基图
export(create_oneheath_dashboard)   # 创建One Health仪表板
export(visualize_transmission_routes) # 可视化传播路径

# 高级可视化
export(create_3d_resistance_landscape) # 创建3D耐药景观
export(build_interactive_network)   # 构建交互网络图
export(generate_animated_timelines) # 创建动画时间线
export(create_multi_panel_display)  # 创建多面板显示

# 其他可视化
export(create_funnel_plot)          # 创建漏斗图
export(create_trend_plot)           # 创建趋势图
export(create_violin_plot)          # 创建小提琴图
export(create_comparison_plot)      # 创建比较图
```

### 5. Shiny应用模块 (shiny)

**目的**: 提供交互式用户界面和应用程序

```r
# 主应用
export(launch_meteor)               # 启动主应用
export(run_module)                  # 运行特定模块
export(create_custom_dashboard)     # 创建自定义仪表板
export(save_dashboard_state)        # 保存仪表板状态

# UI组件
export(forest_plot_module)          # 森林图模块
export(map_module)                  # 地图模块
export(trend_module)                # 趋势模块
export(oneheath_comparison_module)  # One Health比较模块
export(filter_sidebar_module)       # 筛选侧边栏

# 用户交互
export(download_handler)            # 下载处理程序
export(reactive_filter)             # 响应式筛选器
export(create_interactive_table)    # 创建交互表格
export(create_tooltip)              # 创建工具提示
```

### 6. 预测与模拟模块 (prediction)

**目的**: 提供预测和模拟功能

```r
# 趋势预测
export(forecast_resistance)         # 预测未来耐药趋势
export(identify_emerging_threats)   # 识别新兴威胁
export(create_forecast_dashboard)   # 创建预测仪表板

# 干预模拟
export(simulate_intervention)       # 模拟干预影响
export(evaluate_policy_scenarios)   # 评估政策方案
export(calculate_resistance_delay)  # 计算耐药延迟
export(optimize_intervention_strategy) # 优化干预策略

# One Health传播分析
export(trace_resistance_flow)       # 分析耐药传播路径
export(identify_transmission_hotspots) # 识别传播热点
export(visualize_gene_sharing)      # 可视化基因共享
export(model_cross_domain_transfer) # 模拟跨域传递
```

### 7. 整合模块 (integration)

**目的**: 连接和整合外部数据源

```r
# 数据库连接
export(connect_to_glass)            # 连接到WHO GLASS
export(import_from_atlas)           # 从ATLAS导入
export(sync_with_ear)               # 与EAR网络同步

# 补充数据整合
export(integrate_antimicrobial_usage) # 整合抗生素使用数据
export(correlate_with_vaccination)  # 与疫苗接种相关分析
export(connect_with_wgs_database)   # 连接基因组数据库
export(integrate_environmental_data) # 整合环境数据

# 文献和更新
export(scan_new_literature)         # 扫描新文献
export(extract_study_data)          # 从文献提取数据
export(monitor_publications)        # 监控新发布研究
```

### 8. 报告模块 (reporting)

**目的**: 生成报告和摘要

```r
# 报告生成
export(generate_summary_report)     # 生成摘要报告
export(generate_region_report)      # 生成区域报告
export(generate_pathogen_report)    # 生成病原体报告
export(generate_local_report)       # 生成本地研究报告

# 特殊报告
export(generate_adaptive_report)    # 生成自适应报告
export(create_policy_brief)         # 创建政策简报
export(generate_visual_abstract)    # 生成视觉摘要
export(generate_executive_summary)  # 生成执行摘要

# 临床应用
export(generate_treatment_guidance) # 生成治疗指南
export(personalize_recommendations) # 个性化建议
export(visualize_success_probability) # 可视化成功率
export(estimate_amr_costs)          # 估算AMR成本

# 多语言支持
export(translate_report)            # 翻译报告
export(create_localized_visualization) # 创建本地化可视化
```

### 9. 工具模块 (utils)

**目的**: 提供辅助工具和函数

```r
# 导出工具
export(export_plot)                 # 导出图表
export(export_data)                 # 导出数据
export(export_interactive)          # 导出交互式内容
export(export_to_ar)                # 导出到AR格式

# 协作工具
export(create_collaborative_project) # 创建协作项目
export(standardize_collection_protocol) # 标准化收集协议
export(track_contributions)         # 跟踪贡献
export(share_analysis_workflow)     # 分享分析工作流

# 参考工具
export(get_who_classification)      # 获取WHO分类
export(get_resistance_breakpoints)  # 获取耐药断点
export(cite_meteor)                 # 生成引文

# 开发者工具
export(create_amr_template)         # 创建AMR模板
export(test_data_compatibility)     # 测试数据兼容性
export(extend_meteor)               # 扩展包功能
```

## 数据模型设计

```r
# 核心数据结构
meteor_dataset <- list(
  # 元数据
  metadata = data.frame(
    study_id = character(),           # 唯一ID
    first_author = character(),       # 第一作者
    publication_year = integer(),     # 发表年份
    country = character(),            # 国家
    region = character(),             # 区域
    continent = character(),          # 大洲
    study_design = character(),       # 研究设计
    sampling_method = character(),    # 采样方法
    sample_size = integer(),          # 样本量
    quality_score = numeric(),        # 质量评分
    data_collection_start = Date(),   # 数据收集开始
    data_collection_end = Date()      # 数据收集结束
  ),
  
  # 人类AMR数据
  human_data = data.frame(
    record_id = character(),          # 记录ID
    study_id = character(),           # 研究ID
    pathogen = character(),           # 病原体
    pathogen_group = character(),     # 病原体组
    antibiotic_class = character(),   # 抗生素类别
    specific_antibiotic = character(),# 具体抗生素
    resistance_rate = numeric(),      # 耐药率
    ci_lower = numeric(),             # 置信区间下限
    ci_upper = numeric(),             # 置信区间上限
    population_type = character(),    # 人群类型
    age_group = character(),          # 年龄组
    setting = character(),            # 环境设置
    previous_hospitalization = logical(), # A=住院史
    previous_antibiotic_use = character() # 抗生素使用史
  ),
  
  # 动物AMR数据
  animal_data = data.frame(
    record_id = character(),          # 记录ID
    study_id = character(),           # 研究ID
    pathogen = character(),           # 病原体
    pathogen_group = character(),     # 病原体组
    antibiotic_class = character(),   # 抗生素类别
    specific_antibiotic = character(),# 具体抗生素
    resistance_rate = numeric(),      # 耐药率
    ci_lower = numeric(),             # 置信区间下限
    ci_upper = numeric(),             # 置信区间上限
    animal_type = character(),        # 动物类型
    animal_category = character(),    # 动物类别
    farming_system = character(),     # 养殖系统
    sample_type = character(),        # 样本类型
    antibiotic_usage = character()    # 抗生素使用情况
  ),
  
  # 环境AMR数据
  environment_data = data.frame(
    record_id = character(),          # 记录ID
    study_id = character(),           # 研究ID
    pathogen = character(),           # 病原体
    pathogen_group = character(),     # 病原体组
    antibiotic_class = character(),   # 抗生素类别
    specific_antibiotic = character(),# 具体抗生素
    detection_rate = numeric(),       # 检出率
    ci_lower = numeric(),             # 置信区间下限
    ci_upper = numeric(),             # 置信区间上限
    sample_type = character(),        # 样本类型
    environment_category = character(),# 环境类别
    proximity_human = character(),    # 人类接近度
    proximity_animals = character(),  # 动物接近度
    pollution_source = character()    # 污染源
  ),
  
  # 分子数据(可选)
  molecular_data = data.frame(
    record_id = character(),          # 记录ID
    study_id = character(),           # 研究ID
    domain = character(),             # 领域
    sample_id = character(),          # 样本ID
    gene_name = character(),          # 基因名称
    gene_family = character(),        # 基因家族
    resistance_mechanism = character(),# 耐药机制
    detection_frequency = numeric(),  # 检出频率
    host_species = character()        # 宿主物种
  ),
  
  # 参考表格
  reference_tables = list(
    # 地理层次
    geo_hierarchy = data.frame(),
    # 病原体分类
    pathogen_classification = data.frame(),
    # 抗生素分类
    antibiotic_classification = data.frame()
  ),
  
  # 分析结果
  analysis_results = list()           # 存储分析结果
)
```

## 用户工作流设计

### 基础工作流

```r
# 基本使用流程
library(meteor)

# 导入数据
amr_data <- import_amr_data("amr_studies.csv")

# 验证和标准化
validated_data <- validate_data(amr_data)
std_data <- standardize_amr_data(validated_data)

# 执行元分析
meta_results <- calculate_pooled_rate(
  data = std_data,
  by = c("pathogen", "antibiotic_class"),
  method = "random"
)

# 创建可视化
forest <- create_forest_plot(meta_results)
map <- create_geo_map(std_data)
heatmap <- create_resistance_heatmap(meta_results)

# 生成报告
generate_summary_report(
  data = std_data,
  results = meta_results,
  plots = list(forest, map, heatmap),
  output_format = "html"
)
```

### 交互式工作流

```r
# 交互式工作流
launch_meteor()  # 启动Shiny应用

# 或运行特定模块
run_module("forest_plot")
run_module("geographic")
```

### 本地数据分析工作流

```r
# 本地数据分析
local_data <- import_local_data("my_hospital_data.csv")
std_local <- standardize_local_data(local_data)

# 与元分析比较
comparison <- compare_with_meta(
  local_data = std_local,
  reference = "global_amr",
  by = c("pathogen", "antibiotic_class")
)

# 可视化比较
plot <- create_comparison_plot(comparison)

# 生成本地报告
generate_local_report(
  local_data = std_local,
  comparison = comparison,
  include_recommendations = TRUE
)
```

### 预测与模拟工作流

```r
# 预测趋势
forecast <- forecast_resistance(
  data = std_data,
  pathogen = "K. pneumoniae",
  antibiotic = "carbapenem",
  horizon = 24,  # 月
  method = "arima"
)

# 干预模拟
intervention <- simulate_intervention(
  data = std_data,
  scenario = "reduce_use",
  parameters = list(reduction = 0.3),
  time_horizon = 5
)
```

## 扩展与集成

### 扩展机制

```r
# 通过插件扩展METEOR
register_meteor_extension(
  name = "clinical_module",
  functions = list(
    calculate_clinical_impact = function(...) { ... },
    estimate_mortality = function(...) { ... }
  ),
  data = list(
    clinical_outcomes = my_data
  )
)
```

### 外部集成

```r
# 与外部系统集成
# 例如WHO GLASS数据库
glass_data <- connect_to_glass(
  credentials = glass_credentials,
  query = list(
    pathogen = "E. coli",
    years = 2018:2022
  )
)

# 合并到METEOR数据集
enhanced_data <- merge_domains(
  meteor_data = std_data,
  external_data = glass_data,
  match_by = c("country", "pathogen", "year")
)
```

## 全面集成示例

```r
# 从导入到报告的完整工作流
library(meteor)

# 1. 数据准备阶段
# 导入多来源数据
human_data <- import_amr_data("human_studies.csv")
animal_data <- import_amr_data("animal_studies.csv")
env_data <- import_amr_data("environment_studies.csv")

# 验证和标准化
validated_human <- validate_data(human_data)
validated_animal <- validate_data(animal_data)
validated_env <- validate_data(env_data)

std_human <- standardize_amr_data(validated_human)
std_animal <- standardize_amr_data(validated_animal)
std_env <- standardize_amr_data(validated_env)

# 合并数据
integrated_data <- merge_domains(
  human = std_human,
  animal = std_animal,
  environment = std_env
)

# 2. 分析阶段
# 执行元分析
meta_results <- calculate_pooled_rate(
  data = integrated_data,
  by = c("domain", "pathogen", "antibiotic_class"),
  method = "random"
)

# 执行亚组分析
subgroup <- perform_subgroup_analysis(
  data = meta_results,
  by = c("region", "time_period")
)

# 检测异常模式
patterns <- detect_resistance_clusters(integrated_data)

# 执行时间趋势分析
trends <- analyze_time_trends(
  data = integrated_data,
  by = c("pathogen", "antibiotic_class"),
  time_variable = "year"
)

# 3. 可视化阶段
# 创建可视化
forest <- create_interactive_forest(meta_results)
map <- create_geo_map(integrated_data)
heatmap <- create_resistance_heatmap(meta_results)
domain_compare <- create_domain_comparison(meta_results)
trend_plot <- create_trend_plot(trends)
sankey <- create_sankey_diagram(integrated_data)

# 4. 预测与模拟阶段
# 预测未来趋势
forecast <- forecast_resistance(
  data = integrated_data,
  pathogen = "K. pneumoniae",
  antibiotic = "carbapenem",
  region = "global",
  horizon = 36
)

# 模拟干预
intervention <- simulate_intervention(
  data = integrated_data,
  scenario = "reduce_hospital_use",
  parameters = list(reduction = 0.3),
  time_horizon = 5
)

# 5. 报告与通信阶段
# 生成综合报告
generate_adaptive_report(
  data = integrated_data,
  results = list(
    meta = meta_results,
    subgroup = subgroup,
    patterns = patterns,
    trends = trends,
    forecast = forecast,
    intervention = intervention
  ),
  plots = list(
    forest = forest,
    map = map,
    heatmap = heatmap,
    domain_compare = domain_compare,
    trend_plot = trend_plot,
    sankey = sankey
  ),
  audience = "researchers",
  output_format = "html"
)

# 生成政策简报
create_policy_brief(
  data = integrated_data,
  results = list(meta_results, forecast, intervention),
  focus = "strategic_priorities",
  language = "english"
)

# 6. 可选: 启动交互式探索
launch_meteor(data = integrated_data)
```

这个全面框架设计使METEOR成为一个强大而灵活的工具，能够满足从基础元分析到高级预测模拟的各种需求。通过模块化设计，既可以分阶段开发实现，又可以让用户根据需要选择性使用功能。随着项目的发展，可以基于用户反馈和新技术不断扩展这个框架。
