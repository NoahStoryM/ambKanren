# ambKanren v3 迁移记录

建议本包版本升为 **3.0**：依赖 typed-amb 3.0，调度由全局 frontier 改为选择点的二元 merger；递归关系显式暂停，TRS1 的旧 sequence 层 `mplusi`、`bindi` 被局部生成搜索替代并不再导出。这些是可观察的语义/API 变化，不适合作为 2.x 的兼容更新。

## 实现

- microKanren、TRS2 的 `suspend` 是 `amb-pause`。调用者通过 `current-amb-merger` 指定策略；默认 append 不提供公平调度，测试和手册中的递归例子明确选择 interleave。
- 四种语言的 `run` 最终都使用 `private/search.rkt` 的 `take-answers`，直接推进 `amb-generate*`。取够答案后不再推进；`run 0` 不运行目标。
- `first-answer` 用生成器循环逐个转发暂停。TRS2 的 once/ifte（因而 conda/condu）和 TRS1 的 ifa/ifu 都使用它，不通过会跳过暂停的 sequence 寻找首答案。
- TRS1 的普通析取/局部合取选 append，交错形式选 alternate，与 amb 仓库的参考层一致。TRS2 首轮保留“合取即函数复合”；后来 TRS2 与 microKanren 的合取都改为局部 bind `(g2 (amb-local (g1 s)))`，与 miniKanren-typed 一致，见“合取改为局部 bind”。
- 只使用 typed/amb、typed/amb/base、typed/amb/merge 的公开接口；没有依赖 amb 私有模块或 prompt tag。移除 typed-data-queue 依赖。
- demo 保留简洁的 TRS1 风格，run 同样直接读取生成器。

## 书中例子与逐例差异

源：miniKanren-typed `c197a5c98c40d8d409b37fa4cc54f135b1b5c7e3`。TRS1 延续现有测试的章节范围（1–8、10，228 例）；TRS2 是 1–9（含 arith，285 例）。没有从依赖仓库复制书中源码。

`tools/book-trs1.rkt`、`tools/book-trs2.rkt` 仍用完整 TR 类型检查，仅把每次书中断言交给有界记录器。`same-answers` 比较含重数的答案袋；`same-count` 仅说明数量相同，不宣称答案相同。不同配置的资源预算不同，不能把这些统计当成相同预算的速度排名。

下表是首轮记录（TRS2 直接合取）；改为局部 bind 后的结果见“合取改为局部 bind”。

| 语言/策略 | 与书逐项相同 | 仅顺序不同 | 仅数量相同 | 超时/资源未完成 |
|---|---:|---:|---:|---:|
| TRS1 append | 228 | 0 | 0 | 0 |
| TRS1 interleave | 228 | 0 | 0 | 0 |
| TRS1 alternate | 228 | 0 | 0 | 0 |
| TRS1 level | 228 | 0 | 0 | 0 |
| TRS2 append（每例 1 秒） | 255 | 6 | 7 | 17 |
| TRS2 interleave（每例 30 秒） | 283 | 2 | 0 | 0 |
| TRS2 alternate（每例 30 秒） | 278 | 7 | 0 | 0 |
| TRS2 level（60 秒筛查，慢例 600 秒复核） | 270 | 9 | 6 | 0 |

TRS1 的局部运算显式选定 append/alternate，所以改变外层参数不等于替换它们的内部策略。

与 amb 仓库迁移前参考测试对照：TRS1 是 228/228，TRS2 interleave 是 283+2，alternate 是 278+7，完全一致。参考层 level 的历史记录为 270+9+6；本次初始 60 秒预算下 ch8:205 未完成；随后用 600 秒单例预算复核，在 293 秒完成、分类 same-answers。复核后的 270+9+6 与参考层完全一致。中间的 60 秒和 180 秒记录仅表示当时未完成。

**原因验证**：临时副本只把 TRS2 `conj2` 改成 `(g2 (amb-local* (λ () (g1 s))))`，interleave 下恢复 **285/285** 与书完全一致；后来采用了这个定义（见“合取改为局部 bind”）。首轮的两处差异 ch8:205、ch9:167 因此来自直接合取与局部 bind 的差别，不是丢解。

逐例列表、分类和原因在 [research/v3-results/DIFFERENCES.md](research/v3-results/DIFFERENCES.md)，各 JSON 文件保存每例分类、耗时，以及不一致答案/期望（长结果记录长度与 SHA1 摘要）。alternate/level/append 与书的策略不同，有限 run 的顺序或前缀改变是预期差别。超时只记为未完成，不以它证明发散，也不据此判断 bug；append 本来没有公平性保证。

复现（先安装本分支与 amb v3；所有命令有外部上限）：

```sh
LC_ALL=C.UTF-8 AMB_MERGER=interleave BOOK_TIMEOUT=30 BOOK_RECORD=trs2.rktd timeout 180s racket tools/book-trs2.rkt
LC_ALL=C.UTF-8 AMB_MERGER=alternate BOOK_TIMEOUT=30 BOOK_RECORD=trs2-alt.rktd timeout 180s racket tools/book-trs2.rkt
LC_ALL=C.UTF-8 AMB_MERGER=append BOOK_TIMEOUT=1 BOOK_RECORD=trs2-append.rktd timeout 120s racket tools/book-trs2.rkt
LC_ALL=C.UTF-8 AMB_MERGER=level BOOK_TIMEOUT=60 BOOK_RECORD=trs2-level.rktd timeout 600s racket tools/book-trs2.rkt
LC_ALL=C.UTF-8 AMB_MERGER=level BOOK_FOCUS=ch8.rktl:205 BOOK_TIMEOUT=600 BOOK_RECORD=level-one.rktd timeout 630s racket tools/book-trs2.rkt
LC_ALL=C.UTF-8 AMB_MERGER=append BOOK_TIMEOUT=30 BOOK_RECORD=trs1.rktd timeout 120s racket tools/book-trs1.rkt
```

## 首轮验证与发布顺序

Racket 9.3 CS、`LC_ALL=C.UTF-8`。v2 与 v3 使用独立 PLTUSERHOME；v2 是 amb `6340911`、ambKanren `ad2586d`，首轮 v3 记录对应本包 `0488eea` 与 amb `48c97bb`。后续可信导入边界的验证与原生比较另列在下节。验证前递归删除两个仓库生成的 compiled 目录。

- 带文档 `raco setup --check-pkg-deps --pkgs amb typed-amb` 和 `--pkgs ambKanren` 均通过，无错误、无警告、无依赖报告。
- `timeout 360s raco test --timeout 120 -p amb typed-amb`：265 个测试通过，含现有 bench 程序；其输出不作为本次性能结论。amb 中四个可选书中模块及孤立的加载器、记录器已删除，kanren 规律测试保留，移除仅为可选书中测试引入的 typed-racket-lib build-dep。
- `timeout 300s raco test --timeout 120 -p ambKanren`：711 个测试通过，无跳过书中例子。TRS2 只有两个具体位置采用 same-answers 比较，其余逐项比较；新增 once 无限暂停的外层公平性、空搜索和 run 0 回归。

CI 临时先从 NoahStoryM/amb 的 v3 分支安装 amb/typed-amb。**amb v3 发布后改回包目录安装**。发布仍由 Noah 操作：先合并 amb #24，等包目录到 3.0，再改回本分支 CI 并合并本 PR。

## 首轮 v2 耗时记录

书中分类记录器按每个例子单独启动线程以约束资源，其耗时不直接当作 v2/v3 性能比。单独的 `tools/test-times.py` 在已编译测试模块上、每次新 Racket 进程中用 time-apply 记录执行 CPU/real/GC，编译不计入；这是单次观察，不是优化或严格的性能 benchmark。

v2 的原始 TRS2 测试在第一轮 60 秒整文件预算下未完成；逐例 20 秒的诊断中 ch6:104、ch8:205 超时。其余 283 例为 269 完全一致、8 仅顺序、6 仅数量。完整测试模块的隔离执行结果如下（毫秒；每项单次采样，预先编译）：

| 测试 | v2 CPU / real / GC | v3 CPU / real / GC | v3 / v2 real |
|---|---:|---:|---:|
| TRS1 | 22489 / 22575 / 3803 | 22778 / 22873 / 3066 | 1.013（约 +1.3%） |
| TRS2 | 180 秒进程上限内未完成 | 17758 / 17805 / 6407 | 无法计算精确比值 |

进程总墙钟另记：TRS1 v2 23.332 秒、v3 23.219 秒；TRS2 v3 18.168 秒。它包含进程启动，和表中 time-apply 的 real 不是同一指标。v2 TRS2 未完成不能解释成具有精确倍数的加速；两版已有不同的书中答案匹配范围，这也限制了性能比较。原始输出保存在 `research/v3-results/performance-v2.jsonl`、`performance-v3.jsonl`。未进行优化、预热重复或中位数统计。

复现时分别在已安装对应版本、已编译测试的隔离包环境中运行（v2 的 REPO 指向 ad2586d 工作树，v3 指向本分支；脚本本身使用本分支的版本）：

```sh
LC_ALL=C.UTF-8 PLTUSERHOME="$V2_HOME" timeout 390s python3 tools/test-times.py --racket "$RACKET" --repo "$V2_REPO" --timeout 180
LC_ALL=C.UTF-8 PLTUSERHOME="$V3_HOME" timeout 390s python3 tools/test-times.py --racket "$RACKET" --repo "$V3_REPO" --timeout 180
```


## 后续：可信 typed 边界与原生实现比较

本轮采用 amb `c0c8fd9`：使用 dotted Merger、库内 unsafe-require/typed/provide，保留 shallow 前端以支持三种客户端模式；移除未使用的通用 Generator 别名，统一 for/amb 宏路径并复用空向量。本包的搜索算法未改变。

这部分在修改后重新验证：先删 compiled，再在 Racket 9.3 CS / LC_ALL=C.UTF-8 下执行两个仓库带文档的 `raco setup --check-pkg-deps`，均通过、无警告或依赖错误。全部测试 amb/typed-amb **271 项通过**，ambKanren **711 项通过**。书中四策略完整分类沿用前面首轮记录，没有重复运行 level 的 293 秒慢例；本轮全部标准书中断言重新通过。

原生基线为 miniKanren-typed `c197a5c`。`tools/native-trs1.rkt`、`tools/native-trs2.rkt` 调用原生实现，并且只包含与 ambKanren 相同的章节：TRS1 228 例（不含第 9 章）、TRS2 285 例（不含第 10 章）。它们的原生严格断言 **513 项通过**。将原生导入放在 test 子模块中，依然只需已有 build-deps。

测量均使用已经编译的模块，一次预运行后，采样三次，每次新启动一个 Racket 进程；先完成原生组，再完成 amb 组，没有与测试或编译并行。预运行主要减少首次文件/页面缓存差异，不是跨进程 JIT 预热。time-apply 内包含模块加载、查询、收集、reification 和断言，不包含 Racket 进程启动；进程总墙钟另存原始 JSONL。

下表单位为毫秒，各指标分别取三次采样的中位数，real 括号内为三次范围。相对比值以同章原生实现的 real 中位数为 1：

| 测试 | 实现 | CPU 中位数 | real 中位数（范围） | GC 中位数 | real / 原生 |
|---|---|---:|---:|---:|---:|
| TRS1 | miniKanren-typed | 15465 | 15517 (15468–15921) | 295 | 1.000 |
| TRS1 | ambKanren v3 | 22200 | 22228 (21477–22624) | 2995 | 1.432 |
| TRS2 | miniKanren-typed | 12196 | 12220 (11876–12261) | 2471 | 1.000 |
| TRS2 | ambKanren v3 | 17208 | 17233 (16326–19028) | 6016 | 1.410 |

本次观测的 real 开销约为 TRS1 **+43.2%**、TRS2 **+41.0%**。TRS2 波动较大；仅三次样本、顺序分组测量，不能承诺所有查询都落在这个比例内，也不能把差额单独归因于导入检查、续延或分配。

两边执行相同的书中查询，但算法语义仍有前文列出的区别：amb TRS2 的直接合取在两例只保留相同答案袋，原生实现则逐项符合书；断言成本也包含在时间内。这是实际书中套件的端到端记录，没有为了性能修改查询或改用局部 bind，也没有做后续优化。

原始采样（含预运行）为 `research/v3-results/performance-native-native.jsonl`、`performance-amb-native.jsonl`。上节 v2 比较属于首次迁移时的历史采样，不与本轮数据混用。重跑前先清理 compiled 并完成带文档 setup，然后在本仓库根目录运行：

```sh
LC_ALL=C.UTF-8 timeout 600s python3 tools/test-times.py --racket "$(command -v racket)" --implementation native --warmups 1 --repeats 3 --timeout 120 > native.jsonl
LC_ALL=C.UTF-8 timeout 600s python3 tools/test-times.py --racket "$(command -v racket)" --implementation amb --warmups 1 --repeats 3 --timeout 120 > amb.jsonl
```

排除 `warmup: true` 的记录后，再对 stdout 中 TIMING 的 CPU/real/GC 统计；每一条 exit 都必须为 0，并检查 stderr 没有断言失败。本次全部采样均符合此条件。

## 后续：合取改为局部 bind

TRS2 的 `conj2` 和 microKanren 的 `conj` 由 `(g2 (g1 s))` 改为 `(g2 (amb-local (g1 s)))`，与 miniKanren-typed 的 bind 一致：g1 先在局部搜索中求解，g2 依次从它的每个答案继续，所以 merger 按 g1 的答案顺序交替执行 g2 的各个分支，g1 的暂停夹在其间，这就是 `append-map-inf` 的结构。直接复合让 g2 在 g1 的各个选择内运行，交替执行的顺序随这些选择的形状而定；它满足 `(conj2 (disj2 a b) g)` 就是 `(disj2 (conj2 a g) (conj2 b g))`，连顺序也相同，书的 bind 则把 `(disj2 a b)` 的答案放在一起处理。手册的“Order of Answers”（TRS2）和“Suspension and the Order of Answers”（microKanren）作了相应说明。原因的诊断和性能分析在 issue #18。

测试：TRS2 的 285 个书中例子、microKanren 的全部例子（含原来按 same-answers、same-count 放宽的 `wrappers.rktl:37`、`:66`）现在都用 `check-equal?` 逐项比较，`known-order-differences` 机制已删除。验证时先删除 compiled，Racket 9.3 CS、`LC_ALL=C.UTF-8`，带文档的 `raco setup --check-pkg-deps --pkgs ambKanren` 通过，无错误、无警告；`raco test -p ambKanren` **711 项通过**。amb 为 `81bf0c9`。

书中例子分类（命令同上，`research/v3-results/v3-trs2-*.json` 已按本次结果更新）：

| 语言/策略 | 与书逐项相同 | 仅顺序不同 | 仅数量相同 | 超时/资源未完成 |
|---|---:|---:|---:|---:|
| TRS2 append（每例 1 秒） | 255 | 6 | 7 | 17 |
| TRS2 interleave（每例 30 秒） | **285** | 0 | 0 | 0 |
| TRS2 alternate（每例 30 秒） | 279 | 6 | 0 | 0 |
| TRS2 level（60 秒筛查，慢例 600 秒复核） | 270 | 9 | 6 | 0 |

与首轮相比，interleave 的 ch8:205、ch9:167 和 alternate 的 ch8:205 变为逐项相同；append 与 level 的分类不变，level 的 ch8:205 在 60 秒筛查中未完成，600 秒单例复核在 338 秒完成，仍为 same-answers。`v3-trs2-bind.json` 是首轮对这个定义的对照记录，现在与默认实现一致。

耗时（同一进程环境中交替运行 3 次，中位数与范围，单位毫秒，脚本为 `research/v3-results/diagnosis/measure.rkt`）：

| 测试 | 直接合取 | 局部 bind | 原生 miniKanren-typed |
|---|---:|---:|---:|
| TRS2 real | 16266 (15702–16401) | 17306 (16534–17332) | 12758 (12426–13130) |
| TRS2 累计分配 | 24.1 GB | 28.4 GB | 6.1 GB |
| microKanren real | 2025 (1905–2412) | 2194 (1891–2197) | — |
| microKanren 累计分配 | 1.67 GB | 1.59 GB | — |

TRS2 的局部 bind 多出约 18% 的分配；耗时差约 6%，与本环境的波动同一量级，不作为结论。

## 后续：失败改用 `amb-fail`

amb `1c47e90` 起，`(amb)` 和空的 `for/amb` 与非空选择一致，经 `make-amb` 并捕获续延；`amb-fail` 直接中止，不捕获。ambKanren 的失败（`fail`、`unify`、`ext-s` 的 occurs check，TRS1、TRS2、microKanren 和 demo）因此由 `(amb)` 改为 `(amb-fail)`，手册中的说法同步修改。事件不变，711 项测试通过。

在 amb `1c47e90` 上同一时段测量（`research/v3-results/diagnosis/measure.rkt`，累计分配是确定的；耗时为 real，`(amb)` 两次、`amb-fail` 三次）：

| 测试 | `(amb)` | `(amb-fail)` |
|---|---:|---:|
| TRS1 | 31.7 GB，18.3–19.9 s | 29.5 GB，16.8–17.0 s |
| TRS2 | 20.3 GB，15.7–15.8 s | 19.8 GB，14.3–15.0 s |
| microKanren | 1.26 GB，1.91–1.96 s | 1.25 GB，1.80–1.98 s |

## TRS1 公开接口整理（#22／#23，同为 3.0）

本次不改版本号，不保留兼容别名。仅整理
`typed/miniKanren/amb/trs1`（及其 `main`、`base` 实现入口）的导出与
`all-aux` 定义；
底层仍使用局部 amb 搜索。内部组合器移到 `main.rkt`，供宏展开使用，
不再通过 `base.rkt` 或主模块导出。

下表逐项覆盖 issue #22 的 TRS1 差异名单。分类 1 是表示差异，分类 2
是与参照库对齐的接口（含 Noah 指定补齐的类型辅助），分类 3 是不再公开的实验／内部接口。
没有把类型不同的函数直接改名来凑齐导出名单。

| 名字 | 分类／处理 | 迁移方式或理由 |
| --- | --- | --- |
| `list->amb`、`sequence->amb` | 1：保留 | 从列表／序列中选择答案，属于 amb 表示 |
| `bind`、`bindi`、`mplus`、`mplusi` | 1：不补流接口 | 参照库操作流及其悬置；本库在目标内部用 amb 和局部搜索实现组合，类型不同 |
| `case∞`、`map∞`、`λF` | 1：不补 | 参照库的流分派、收集、悬置构造 |
| `Streamof`、`Suspension` | 1：不补 | 参照库的流类型 |
| `empty-stream`、`stream`、`stream-cons` | 1：不补 | 参照库的流构造 |
| `stream-empty?`、`stream-singleton?`、`stream-first`、`stream-rest` | 1：不补 | 参照库的流访问 |
| `conj`、`conji` | 3：不再导出 | 通常改用 `all`、`alli`；旧函数先求值全部参数，新形式在构造时求值首目标、应用时构造其余目标，并非任意上下文的直接替换 |
| `disj`、`disji` | 3：不再导出 | 通常改用 `conde`、`condi`，每个目标各占一个子句；同样注意目标构造的求值时机 |
| `conj+`、`conji+` | 3：不再导出 | `all`、`alli` |
| `disj+`、`disji+` | 3：不再导出 | `(conde [g] ...)`、`(condi [g] ...)` |
| `Zzz` | 3：不再导出 | 需要显式延迟时写 `(λG (s) (g s))`；它从来不是 TRS1 的暂停操作 |
| `apply-goal` | 3：不再导出 | 直接写 `(g s)` |
| `apply-s` | 3：不再导出 | 旧的私有查询辅助；通常用 `walk`／`walk*`，它们会继续解引用 |
| `s-empty?`、`s-has-key?` | 3：不再导出 | 私有辅助；#24 后代换是关联表，可用 `null?`、`assq` 检查其表示 |
| `var=?` | 3：不再导出 | TRS1 逻辑变量用 `eq?` 比较身份 |
| `if-aux` | 3：不再导出 | 用 `ife`／`ifi` 等公开形式 |
| `project` | 3：按 Noah 已定移出接口 | 只在练习测试 `tests/interface.rkt` 定义；TRS2 的 `project` 不变 |
| `run∞` | 3：删除别名 | `run*` |
| `≡`、`∃`、`⊤`、`⊥` | 3：删除别名 | 分别用 `==`、`fresh`、`succeed`、`fail` |
| `∧`、`∧i`、`∨`、`∨i` | 3：删除别名 | 分别用 `all`、`alli`、单目标子句的 `conde`、`condi` |
| `Literal`、`Data`、`data?` | 2：按 Noah 已定补齐 | 类型定义与谓词和 miniKanren-typed 相同；`Term` 改为引用 `Literal`，可接受的项不变 |

共有名字也不意味着类型完全相同：例如 `Goal`、`Substitution`、`unify`、
`ext-s` 仍遵循 amb／哈希表示。

`all-aux` 的定义及 `all`／`alli` 的调用形式与
miniKanren-typed 完全一致：空目标返回 `succeed`，单目标原样返回，多个目标
在构造时求值首目标，应用时递归构造剩余目标。它接受绑定运算符 `bnd`，
不再接受旧版的合取形式参数。与参照库的表示差异集中在私有的 `bind`／
`bindi` 语法：它们捕获答案表达式，在 append／alternate 的局部 amb
搜索上应用后继目标；不导出与流绑定函数同名但类型不同的接口。

microKanren 与 demo 自己定义 `conj+`、`disj+`、`Zzz`、`var=?`，并不从
TRS1 导入；本次不改它们。`suspend`／`pause`、microKanren 的 `take`／
`take-all` 也属于其他入口，本次只在 PR 导出复查中报告，不作接口决定。
`NOTICE` 保留这些定义的来源说明，并记录 TRS1 组合器所在文件与测试中的
`project`。

### #23 的验证（历史记录）

Racket 9.3 CS，所有 Racket 命令均设置 `LC_ALL=C.UTF-8`。
依赖：amb master `1223459`、miniKanren-typed master `c197a5c`；本分支从
ambKanren master `d97a0b6` 开出。版本仍为 3.0。

- 对 microKanren、TRS1、TRS2、demo 的全部 `tests` 目录执行
  `raco test --timeout 180`：**788 tests passed**。
- 沿用既有书中例子：TRS1 第 1–8、10 章，TRS2 第 1–9 章，demo 既有范围；
  没有新增跳过。TRS1 第 9 章原本未整体引入（替换表示不同），本轮另将
  `project` 的练习定义和对应书中断言放入 `tests/interface.rkt`。
- `raco setup --pkgs amb miniKanren-typed ambKanren`：退出码 0，
  三份手册均构建、渲染完成，无未定义交叉引用或警告。
- 用 `module->exports` 合并变量与语法导出，只取 phase 0，复查结果：
  TRS1 共有 40 个名字，本库独有 `list->amb`、`sequence->amb`；参照库
  独有的 16 个名字均为上述流接口。
  TRS2 共有 36 个名字，本库独有 `suspend`；microKanren 共有 32 个名字，
  本库独有 `suspend`、`take`、`take-all`，均保持原样。


## 非搜索表示对齐（#24，同为 3.0）

本轮从 #23 合并后的 master `ece8b5c` 开始。TRS1、TRS2 直接采用
miniKanren-typed `c197a5c` 的 `Literal`、`Data`、`Term`、关联表
`Substitution`、空代换、查找、出现检查、统一化及 reify 定义。
TRS1 的 `size-s` 是 `length`，TRS2 的 `reify-s` 也按关联表长度起名。
TRS2 同时补齐了参照库的 `Literal`、`Data`、`data?`。

非搜索定义只保留失败表示的差别：`ext-s`、`unify` 不返回 `#f`，而是
调用 `amb-fail`，对应返回类型去掉 `Option`；`data?` 为真的扩展与
参照库一样不运行出现检查。目标、选择、暂停和答案收集继续使用 amb。
逐定义的原文对照、所有剩余差异及理由见
[ALIGNMENT.md](research/v3-results/alignment-20261116/ALIGNMENT.md)，
`forms.json` 和三个 `.diff` 文件保留可检查的源码证据。

TRS1 的第 9 章已直接 include 参照库同一份 `ch9.rktl`，测试、
`tools/native-trs1.rkt` 与逐例记录器 `tools/book-trs1.rkt` 现在都覆盖
第 1–10 章。TRS1 两边各 254 个启用的断言，TRS2 两边各 285 个，
均使用原始 `check-equal?`，没有答案袋替代或顺序例外。参照测试文件
自身已有的 `#;` 注释保留原样，没有新增跳过。两份原生入口加本库全部
测试目录合计 **1353 tests passed**，逐项输出见同目录 `validation.txt`。

microKanren 的非搜索代码已与参照库相同，本轮没有修改。
**未定：demo 是否也要对齐。** 它已是关联表，但仍有自定义变量比较、
无 `data?` 捷径的扩展、不同的辅助写法及直接合取；没有参照库的同名
版本。建议保留为独立最小示例，不计入两版性能表。是否进一步对齐由
Noah 决定，本轮不改 demo。

新的交替测量、原始样本、环境及中位数／范围另存于
[alignment-20261116](research/v3-results/alignment-20261116/README.md)。
手册 Performance 使用这份数据；之前的性能记录属于各自历史源码与
环境，不可当作本轮同机消融对照，也没有删除。
