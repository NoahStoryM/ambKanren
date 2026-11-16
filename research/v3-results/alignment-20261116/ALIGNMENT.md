# 逐定义源码对照（#24）

参照：miniKanren-typed `c197a5c`。对照的是源文本，不只比较等价的 s-expression。
`tools/alignment-diff.rkt` 用 read-syntax 的位置保存每个顶层定义及类型标注的原文；
`forms.json` 保存全部比较，三个 `.diff` 文件保存每个不相同定义的 unified diff。
类型标注和函数体分别记录；下面每行同时覆盖同名的定义、类型、宏及标注。所有不同定义都已归类，脚本生成时检查没有遗漏。

## trs1

| 剩余差异 | 涉及名字（仅列实际不同的形式） | 原因 |
| --- | --- | --- |
| 失败表示 | `ext-s`、`unify` | `#f` 改为 `amb-fail`；结果类型去掉 `Option`。其余分支保持参照写法，包括 TRS1 的嵌套 `cond`。 |
| 目标类型 | `Goal` | 从代换到代换；答案由 amb 回溯产生。 |
| 显式流层 | `Streamof`、`Suspension`、`case∞`、`empty-stream`、`map∞`、`mplus`、`mplusi`、`stream`、`stream-cons`、`stream-empty?`、`stream-first`、`stream-rest`、`stream-singleton?`、`λF` | 只在参照库存在；本库由 amb 生成器、merger 和答案收集器承担。 |
| 绑定与内部组合器 | `bind`、`bindi`、`conj`、`conj+`、`conji`、`conji+`、`disj`、`disj+`、`disji`、`disji+` | 参照库的 bind/bindi 是流函数；本库私有绑定形式及内部目标组合器使用 append/alternate 的局部搜索，不导出为流 API。 |
| 构造目标与延迟 | `Zzz`、`fresh`、`λG` | fresh/Zzz 以 Goal thunk 延迟目标构造；λG 只差源码换行（类型标注相同）。 |
| 条件搜索 | `if-aux`、`ifa`、`ife`、`ifi`、`ifu` | 深度优先／交错选择用 amb merger；提交选择用 first-answer 转发暂停并保留后续答案。 |
| 运行与基本目标 | `==`、`fail`、`run`、`succeed` | run 用 take-answers 有界收集，统一化直接返回代换，失败调用 amb-fail，成功返回代换。 |
| amb 选择辅助 | `list->amb` | 本库专有的列表选择函数；sequence->amb 通过 provide 从 typed/amb 重新导出。 |

非搜索类型／定义逐字相同：`define-predicate:data?`、`define-type:Data`、`define-type:Literal`、`define-type:Substitution`、`define-type:Term`、`define:empty-s`、`define:occurs?`、`define:reify`、`define:reify-name`、`define:reify-s`、`define:size-s`、`define:walk`、`define:walk*`、`struct:var`。

## trs2

| 剩余差异 | 涉及名字（仅列实际不同的形式） | 原因 |
| --- | --- | --- |
| 失败表示 | `ext-s`、`unify` | `#f` 改为 `amb-fail`；结果类型去掉 `Option`。 |
| 目标类型 | `Goal` | 从代换到代换，替代从代换到流。 |
| 显式流层 | `Maybe`、`Pause`、`Streamof`、`Suspension`、`append-inf`、`append-map-inf`、`empty-stream`、`pause`、`pause?`、`stream`、`stream-cons`、`stream-empty?`、`stream-first`、`stream-force`、`stream-lazy`、`stream-rest`、`take-inf` | 参照库的流、暂停值、流合并、绑定与收集；在本库由 amb 的生成器／merger 承担。 |
| 基本目标与选择 | `==`、`conj2`、`disj2`、`fail`、`succeed` | 直接返回代换或以 amb-fail 失败；析取是 amb 选择，合取在局部搜索的答案上运行后继目标。 |
| 暂停 | `defrel`、`suspend` | stream-lazy 换成 amb-pause；保留每次关系调用的暂停位置。 |
| 提交选择 | `ifte`、`once` | first-answer 查首答案并转发暂停；ifte 反射其余答案，once 仅保留首答案。 |
| 运行 | `run-goal` | take-answers 直接推进生成器，代替 take-inf。 |

非搜索类型／定义逐字相同：`define-predicate:data?`、`define-type:Data`、`define-type:Literal`、`define-type:Substitution`、`define-type:Term`、`define:call/fresh`、`define:empty-s`、`define:occurs?`、`define:reify`、`define:reify-name`、`define:reify-s`、`define:walk`、`define:walk*`、`struct:var`。

## microKanren

| 剩余差异 | 涉及名字（仅列实际不同的形式） | 原因 |
| --- | --- | --- |
| 失败表示 | `unify` | 失败改用 amb-fail，类型去掉 Option；递归统一化去掉对 #f 的显式传播。 |
| 目标类型 | `Goal`、`call/goal` | Goal 返回 State；call/goal 的返回类型随之改变，函数体相同。 |
| 显式流层 | `Maybe`、`Pause`、`Streamof`、`Suspension`、`bind`、`empty-stream`、`mplus`、`pause`、`pause?`、`stream`、`stream->list`、`stream-cons`、`stream-empty?`、`stream-first`、`stream-force`、`stream-force-take`、`stream-lazy`、`stream-rest`、`stream-take` | 参照库显式流／暂停及组合器在本库由 amb 表示替代。 |
| 基本目标与选择 | `==`、`conj`、`disj` | 目标直接返回状态；析取使用 amb，合取使用局部 amb 搜索。 |
| 暂停 | `Zzz`、`suspend` | stream-lazy 改为 suspend（amb-pause）。 |
| 运行与收集 | `run`、`run*`、`take`、`take-all` | take-answers 代替流收集；take/take-all 接受答案 thunk。 |

非搜索类型／定义逐字相同：`define-type:Literal`、`define-type:State`、`define-type:Substitution`、`define-type:Term`、`define:call/fresh`、`define:call/goal`、`define:empty-state`、`define:ext-s`、`define:fresh/nf`、`define:reify-1st`、`define:reify-name`、`define:reify-s`、`define:var=?`、`define:walk`、`define:walk*`、`struct:var`。

## 文件与导入／导出差异

- TRS1 为沿用现有布局，仍拆为 `private/types.rkt`、`base.rkt`、`main.rkt`；参照库放在一个文件里。重排不改变定义原文。
- 本库 require `typed/amb`、TRS1 的 `typed/amb/merge` 和 `private/search.rkt`，替代显式流层。TRS1 的 `racket/match` 导入沿用旧代码，没有新的公开绑定。
- TRS1 继续使用 #23 的显式导出名单和 `sequence->amb` 重导出；其余入口沿用 all-defined-out。`project` 来自同一份第 9 章练习，不成为 TRS1 公开接口。
- 注释、段落标题、来源说明随文件布局与 amb 搜索说明不同，不属于算法差异。
- 原先私有的哈希表辅助、`var=?`、`apply-goal` 从 TRS1 base 删除；它们自 #23 起已不公开，也不再被核心定义使用。

## microKanren 与 demo

microKanren 的非搜索部分（项、关联表代换、计数器状态、变量比较、walk、ext-s、reify 及辅助）已逐字相同；本轮不改它。上表完整列出既有的搜索／失败差异。

**未定：demo 是否进一步对齐。** demo 已用关联表，但仍用 `assoc` 配合 `var=?`、总做出现检查、部分不同的 cond/if/match 写法及最简目标语言；其合取是直接函数复合。它没有 miniKanren-typed 的独立同名版本。建议把它继续作为独立最小示例，排除在两版本的性能比较之外；是否改为 TRS1 同源代码由 Noah 决定。本 PR 不修改 demo。
