# 1. 加载包（首次运行需先install.packages）
library(igraph)
library(tidygraph)
library(ggraph)
library(readxl)
library(ggplot2)  # 需要加载ggplot2来调整主题

# 2. 读取边列表数据（自动识别Sheet名"Edges"）
edges <- read_excel("D:/pageRank/Edges.xlsx")

# 3. 创建有向网络图（直接使用边列表，无需节点列表）
net <- graph_from_data_frame(
  d = edges,
  directed = TRUE  # 有向图
)

# 4. 计算PageRank（权重=Weight列）
page_rank <- page_rank(net, directed = TRUE, weights = E(net)$Weight)$vector
page_rank_df <- data.frame(Drug = names(page_rank), PageRank = page_rank)
page_rank_df <- page_rank_df[order(-page_rank_df$PageRank), ]  # 按重要性排序

# 5. 输出核心药物排名
print(page_rank_df)

# 6. 可视化网络图（调整图例大小）
set.seed(123)  # 固定布局
ggraph(net, layout = "fr") +  # 力导向布局
  geom_edge_link(aes(width = Weight), color = "grey80", alpha = 0.8) +
  geom_node_point(aes(size = page_rank, color = page_rank)) +
  geom_node_text(aes(label = name), repel = TRUE, size = 4) +
  scale_edge_width(range = c(0.5, 2), name = "Weight") +  # 边粗细范围
  scale_size(range = c(3, 10), name = "PageRank") +       # 节点大小范围
  scale_color_gradient(name = "PageRank") +               # 颜色图例名称
  guides(
    size = guide_legend(title = "PageRank", title.theme = element_text(size = 8), 
                        label.theme = element_text(size = 6), keyheight = 0.5),
    color = guide_colorbar(title = "PageRank", title.theme = element_text(size = 8), 
                           label.theme = element_text(size = 6), barheight = 3),
    edge_width = guide_legend(title = "Weight", title.theme = element_text(size = 8), 
                              label.theme = element_text(size = 6), keyheight = 0.5)
  ) +
  theme_void() +
  theme(
    legend.title = element_text(size = 8),      # 图例标题大小
    legend.text = element_text(size = 6),       # 图例文字大小
    legend.key.size = unit(0.3, "cm"),          # 图例键大小
    legend.position = "right"                   # 图例位置
  )