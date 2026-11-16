# 逐例答案差异

同一答案袋按出现次数比较；same-count 只表示数量相同。超时不等于发散证明。“原因／判断”一列是与观察相容的解释，不是已经证明的唯一原因：策略之外，合取的结构也参与调度。

TRS2 的合取是局部 bind `(g2 (amb-local (g1 s)))`。直接合取 `(g2 (g1 s))` 下另有 interleave 的 ch8:205、ch9:167 和 alternate 的 ch8:205 三处只差顺序，见 [MIGRATION-V3.md](../../MIGRATION-V3.md) 的“合取改为局部 bind”。

| 配置 | 例子 | 分类 | 原因／判断 |
|---|---|---|---|
| alternate | ch1.rktl:246 | same-answers | 答案也切换执行侧，区别于书的暂停交织；答案袋相同，属预期差别。 |
| alternate | ch3.rktl:213 | same-answers | 答案也切换执行侧，区别于书的暂停交织；答案袋相同，属预期差别。 |
| alternate | ch7.rktl:100 | same-answers | 答案也切换执行侧，区别于书的暂停交织；答案袋相同，属预期差别。 |
| alternate | ch8.rktl:92 | same-answers | 答案也切换执行侧，区别于书的暂停交织；答案袋相同，属预期差别。 |
| alternate | ch8.rktl:122 | same-answers | 答案也切换执行侧，区别于书的暂停交织；答案袋相同，属预期差别。 |
| alternate | ch9.rktl:167 | same-answers | 答案也切换执行侧，区别于书的暂停交织；答案袋相同，属预期差别。 |
| append | ch1.rktl:242 | same-answers | append 深度优先改变 枚举顺序；答案袋相同，属预期差别。 |
| append | ch1.rktl:256 | same-answers | append 深度优先改变 枚举顺序；答案袋相同，属预期差别。 |
| append | ch1.rktl:260 | same-answers | append 深度优先改变 枚举顺序；答案袋相同，属预期差别。 |
| append | ch3.rktl:115 | same-count | 有界 run 的前缀随append 的深度优先变化，与预期相容；数量分类本身不证明完整性。 |
| append | ch3.rktl:119 | same-count | 有界 run 的前缀随append 的深度优先变化，与预期相容；数量分类本身不证明完整性。 |
| append | ch3.rktl:213 | same-count | 有界 run 的前缀随append 的深度优先变化，与预期相容；数量分类本身不证明完整性。 |
| append | ch4.rktl:160 | same-answers | append 深度优先改变 枚举顺序；答案袋相同，属预期差别。 |
| append | ch4.rktl:188 | same-answers | append 深度优先改变 枚举顺序；答案袋相同，属预期差别。 |
| append | ch4.rktl:192 | timeout | 预算内未完成；未判定答案差异，不能据此断言 bug 或发散。 |
| append | ch4.rktl:196 | timeout | 预算内未完成；未判定答案差异，不能据此断言 bug 或发散。 |
| append | ch4.rktl:200 | timeout | 预算内未完成；未判定答案差异，不能据此断言 bug 或发散。 |
| append | ch4.rktl:208 | timeout | 预算内未完成；未判定答案差异，不能据此断言 bug 或发散。 |
| append | ch4.rktl:216 | timeout | 预算内未完成；未判定答案差异，不能据此断言 bug 或发散。 |
| append | ch6.rktl:52 | timeout | 预算内未完成；未判定答案差异，不能据此断言 bug 或发散。 |
| append | ch6.rktl:61 | timeout | 预算内未完成；未判定答案差异，不能据此断言 bug 或发散。 |
| append | ch6.rktl:80 | timeout | 预算内未完成；未判定答案差异，不能据此断言 bug 或发散。 |
| append | ch6.rktl:88 | timeout | 预算内未完成；未判定答案差异，不能据此断言 bug 或发散。 |
| append | ch6.rktl:95 | timeout | 预算内未完成；未判定答案差异，不能据此断言 bug 或发散。 |
| append | ch6.rktl:104 | timeout | 预算内未完成；未判定答案差异，不能据此断言 bug 或发散。 |
| append | ch7.rktl:100 | same-count | 有界 run 的前缀随append 的深度优先变化，与预期相容；数量分类本身不证明完整性。 |
| append | ch8.rktl:8 | same-count | 有界 run 的前缀随append 的深度优先变化，与预期相容；数量分类本身不证明完整性。 |
| append | ch8.rktl:92 | same-count | 有界 run 的前缀随append 的深度优先变化，与预期相容；数量分类本身不证明完整性。 |
| append | ch8.rktl:108 | timeout | 预算内未完成；未判定答案差异，不能据此断言 bug 或发散。 |
| append | ch8.rktl:122 | same-count | 有界 run 的前缀随append 的深度优先变化，与预期相容；数量分类本身不证明完整性。 |
| append | ch8.rktl:205 | timeout | 预算内未完成；未判定答案差异，不能据此断言 bug 或发散。 |
| append | ch8.rktl:218 | timeout | 预算内未完成；未判定答案差异，不能据此断言 bug 或发散。 |
| append | ch9.rktl:107 | same-answers | append 深度优先改变 枚举顺序；答案袋相同，属预期差别。 |
| append | ch9.rktl:154 | timeout | 预算内未完成；未判定答案差异，不能据此断言 bug 或发散。 |
| append | ch9.rktl:167 | timeout | 预算内未完成；未判定答案差异，不能据此断言 bug 或发散。 |
| append | ch9.rktl:186 | timeout | 预算内未完成；未判定答案差异，不能据此断言 bug 或发散。 |
| level | ch3.rktl:115 | same-answers | 同步暂停轮次改变 答案顺序；答案袋相同，属预期差别。 |
| level | ch3.rktl:119 | same-answers | 同步暂停轮次改变 答案顺序；答案袋相同，属预期差别。 |
| level | ch3.rktl:213 | same-count | 有界 run 的前缀随同步暂停轮次变化，与预期相容；数量分类本身不证明完整性。 |
| level | ch7.rktl:100 | same-count | 有界 run 的前缀随同步暂停轮次变化，与预期相容；数量分类本身不证明完整性。 |
| level | ch7.rktl:127 | same-answers | 同步暂停轮次改变 答案顺序；答案袋相同，属预期差别。 |
| level | ch7.rktl:136 | same-answers | 同步暂停轮次改变 答案顺序；答案袋相同，属预期差别。 |
| level | ch8.rktl:8 | same-answers | 同步暂停轮次改变 答案顺序；答案袋相同，属预期差别。 |
| level | ch8.rktl:92 | same-count | 有界 run 的前缀随同步暂停轮次变化，与预期相容；数量分类本身不证明完整性。 |
| level | ch8.rktl:108 | same-count | 有界 run 的前缀随同步暂停轮次变化，与预期相容；数量分类本身不证明完整性。 |
| level | ch8.rktl:122 | same-answers | 同步暂停轮次改变 答案顺序；答案袋相同，属预期差别。 |
| level | ch8.rktl:155 | same-answers | 同步暂停轮次改变 答案顺序；答案袋相同，属预期差别。 |
| level | ch8.rktl:160 | same-count | 有界 run 的前缀随同步暂停轮次变化，与预期相容；数量分类本身不证明完整性。 |
| level | ch8.rktl:205 | same-answers | 600 秒单例复核在 338 秒完成；答案袋相同，顺序不同，属同步轮次的预期差别。 |
| level | ch9.rktl:167 | same-answers | 同步暂停轮次改变 答案顺序；答案袋相同，属预期差别。 |
| level | ch9.rktl:186 | same-count | 有界 run 的前缀随同步暂停轮次变化，与预期相容；数量分类本身不证明完整性。 |
