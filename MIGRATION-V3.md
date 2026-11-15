# ambKanren v3 迁移记录

建议本包版本升为 **3.0**：依赖 typed-amb 3.0，调度由全局 frontier 改为选择点的二元 merger；递归关系显式暂停，TRS1 的旧 sequence 层 `mplusi`、`bindi` 被局部生成搜索替代并不再导出。这些是可观察的语义/API 变化，不适合作为 2.x 的兼容更新。

## 实现

- microKanren、TRS2 的 `suspend` 是 `amb-pause`。调用者通过 `current-amb-merger` 指定策略；默认 append 不提供公平调度，测试和手册中的递归例子明确选择 interleave。
- 四种语言的 `run` 最终都使用 `private/search.rkt` 的 `take-answers`，直接推进 `amb-generate*`。取够答案后不再推进；`run 0` 不运行目标。
- `first-answer` 用生成器循环逐个转发暂停。TRS2 的 once/ifte（因而 conda/condu）和 TRS1 的 ifa/ifu 都使用它，不通过会跳过暂停的 sequence 寻找首答案。
- TRS1 的普通析取/局部合取选 append，交错形式选 alternate，与 amb 仓库的参考层一致。TRS2 保留现有“合取即函数复合”；没有悄悄改成书的 bind。
- 只使用 typed/amb、typed/amb/base、typed/amb/merge 的公开接口；没有依赖 amb 私有模块或 prompt tag。移除 typed-data-queue 依赖。
- demo 保留简洁的 TRS1 风格，run 同样直接读取生成器。

## 书中例子与逐例差异

源：miniKanren-typed `c197a5c98c40d8d409b37fa4cc54f135b1b5c7e3`。TRS1 延续现有测试的章节范围（1–8、10，228 例）；TRS2 是 1–9（含 arith，285 例）。没有从依赖仓库复制书中源码。

`tools/book-trs1.rkt`、`tools/book-trs2.rkt` 仍用完整 TR 类型检查，仅把每次书中断言交给有界记录器。`same-answers` 比较含重数的答案袋；`same-count` 仅说明数量相同，不宣称答案相同。不同配置的资源预算不同，不能把这些统计当成相同预算的速度排名。

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

**原因验证**：临时副本只把 TRS2 `conj2` 改成 `(g2 (amb-local* (λ () (g1 s))))`，interleave 下恢复 **285/285** 与书完全一致；未把这个变体作为新的公共 API。默认两处差异 ch8:205、ch9:167 因此来自直接合取与局部 bind 的差别，不是丢解。

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

## 验证与发布顺序

Racket 9.3 CS、`LC_ALL=C.UTF-8`。v2 与 v3 使用独立 PLTUSERHOME；v2 是 amb `6340911`、ambKanren `ad2586d`，v3 是本分支与 amb 的 v3 分支。验证前递归删除两个仓库生成的 compiled 目录。

- 带文档 `raco setup --check-pkg-deps --pkgs amb typed-amb` 和 `--pkgs ambKanren` 均通过，无错误、无警告、无依赖报告。
- `timeout 360s raco test --timeout 120 -p amb typed-amb`：265 个测试通过，含现有 bench 程序；其输出不作为本次性能结论。amb 中四个可选书中模块及孤立的加载器、记录器已删除，kanren 规律测试保留，移除仅为可选书中测试引入的 typed-racket-lib build-dep。
- `timeout 300s raco test --timeout 120 -p ambKanren`：711 个测试通过，无跳过书中例子。TRS2 只有两个具体位置采用 same-answers 比较，其余逐项比较；新增 once 无限暂停的外层公平性、空搜索和 run 0 回归。

CI 临时先从 NoahStoryM/amb 的 v3 分支安装 amb/typed-amb。**amb v3 发布后改回包目录安装**。发布仍由 Noah 操作：先合并 amb #24，等包目录到 3.0，再改回本分支 CI 并合并本 PR。

## 耗时记录

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
