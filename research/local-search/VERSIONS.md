# TRS2 的几个 ambKanren 实现：对照

研究用（[#14](https://github.com/NoahStoryM/ambKanren/issues/14)），不合入正式代码。所有变体都保持 `Goal : Substitution → Substitution` 与直接风格，接口与 `miniKanren/amb/trs2/main.rkt` 相同，都能直接 `include` 书中例子。

## 生成链

除基线外，每个变体都由脚本从上一级**生成**，只替换「局部搜索」那一节和少数定义，其余代码逐字相同。所以对比代码时直接 `diff` 相邻两级即可：

```
miniKanren/amb/trs2/main.rkt          基线：全局前沿
  └─ gen-trs2-local.rkt → trs2-local.rkt          局部组合子，单独的 prompt tag，未融合
       ├─ gen-exn.rkt   → trs2-exn.rkt            同上，暂停改用 raise / with-handlers
       │    └─ gen-shift.rkt → trs2-shift.rkt     不用 amb：shift/reset 直接 reify 成流；conj2 为函数复合，每个 conj 宏与关系体划界
       └─ gen-fused.rkt → trs2-local-fused.rkt    加融合律 reify (lift f) = f
            ├─ gen-amb-plain.rkt    → trs2-amb-plain.rkt     不让 amb 看到暂停（reflect 强制）
            ├─ gen-amb-pause.rkt    → trs2-amb-pause.rkt     amb 的 pause / in-amb/pause
            ├─ gen-empty-values.rkt → trs2-empty-values.rkt  amb 的 current-amb-empty-values
            ├─ gen-amb-output.rkt   → trs2-amb-output.rkt    amb 的 amb-output
            └─ gen-amb-empty.rkt    → trs2-amb-empty.rkt     amb 的 amb-fail / amb-empty
miniKanren/amb/trs2/main.rkt
  └─ gen-graded.rkt → trs2-graded.rkt     满足定律的分层语义：直接风格 + 迭代加深（规格）
```

```sh
cd research/local-search
diff ../../miniKanren/amb/trs2/main.rkt trs2-graded.rkt  # 分层语义从哪里来
diff ../../miniKanren/amb/trs2/main.rkt trs2-local.rkt   # 局部组合子从哪里来
diff trs2-local.rkt trs2-exn.rkt                         # prompt tag 换成异常
diff trs2-local.rkt trs2-local-fused.rkt                 # 融合
diff trs2-local-fused.rkt trs2-amb-plain.rkt             # 去掉暂停效应
diff trs2-local-fused.rkt trs2-amb-empty.rkt             # 改用 amb-empty
```

重新生成全部变体，结果与仓库中的文件逐字节相同（2026-11-09 核对）：

```sh
cp ../../miniKanren/amb/trs2/main.rkt trs2-local.rkt && racket gen-trs2-local.rkt trs2-local.rkt
racket gen-fused.rkt trs2-local.rkt trs2-local-fused.rkt
racket gen-exn.rkt   trs2-local.rkt trs2-exn.rkt
racket gen-shift.rkt trs2-exn.rkt trs2-shift.rkt
racket gen-graded.rkt ../../miniKanren/amb/trs2/main.rkt trs2-graded.rkt
for v in amb-plain amb-pause empty-values amb-output amb-empty; do
  racket gen-$v.rkt trs2-local-fused.rkt trs2-$v.rkt
done
```

## 暂停在各版本中的形态

| 版本 | 暂停（`suspend`） | 局部搜索（`reify-goal`） | 需要的 amb |
| --- | --- | --- | --- |
| 基线 | `(parameterize ([current-amb-depth-first? #f]) (amb (void)))` 再 `(amb (void))`：把剩余计算放到全局前沿后面 | 无 | master |
| `trs2-local` | `(when (amb #t #f) (abort-current-continuation pause-tag))` | `in-amb/do` 里装 `pause-tag` 的 prompt | master |
| `trs2-exn` | `(when (amb #t #f) (raise PAUSE))` | `in-amb/do` 里装 `with-handlers` | master |
| `trs2-shift` | `defrel` = `(reflect (λ () (reify-goal body s)))` | `(reset (list (g s)))`，没有序列 | 不需要 |
| `trs2-graded` | 代价加一，超出本层预算则剪枝 | 无（`ifte` / `once` 除外）；`run-goal` 迭代加深 | master |
| `trs2-local-fused` | 同 `trs2-local` | 同上；lifted goal 直接取流函数 | master |
| `trs2-amb-plain` | 无：`reflect` 遇到暂停就强制 | 普通 `in-amb/do`；`==` 等给成熟流，手写 goal 惰性 | master |
| `trs2-amb-pause` | `(pause)` | `in-amb/pause` | `research/observable-pause-20261109` |
| `trs2-empty-values` | `(parameterize ([current-amb-empty-values (list PAUSED)]) (amb))` | 普通 `in-amb/do`，识别 `paused?` | `research/amb-empty-values-20261109` |
| `trs2-amb-output` | `(when (amb #t #f) (amb-output PAUSED))` | 同上 | `research/amb-output-20261109` |
| `trs2-amb-empty` | `(when (amb #t #f) (amb-empty))` | `(in-amb/do (g s) #:empty-values (list PAUSED))` | `research/amb-empty-20261109` |

## 满足定律的版本（分层语义）

`trs2-graded.rkt` 按 Spivey 的分层语义实现：答案按路径上的关系调用次数（代价）分层，层内深度优先。`disj2 = amb`，`conj2` 为函数复合，`==` / `fail` 为 `(amb)`，关系调用把代价加一（存在状态里），`run-goal` 逐层迭代加深。没有流、没有暂停值、没有 raise；只有 `ifte` / `once` 仍需局部搜索（取「有无答案」与「代价最小的答案」）。

- `laws-*.rkt` → `laws.out`：Astra 在 #68 给出的反例写成目标。原书与 `trs2-shift` 的分配律、合取结合律两侧顺序不同；`trs2-graded` 与基线两侧相同。
- `rec-*.rkt` → `rec-*.out`：逐项记录书中例子的实际结果。`trs2-graded` 在 1500 s 内跑完前 269 项（停在 ch8:205，迭代加深重复计算太多），**与基线逐项、逐个答案完全相同**；基线与原书有 15 项顺序不同（即基线测试里放宽的 15 项）。

也就是说，现有的全局前沿基线（暂停 = 把剩余计算排到 amb 前沿末尾）在这些例子上正好实现了分层语义；`trs2-graded` 是它的规格，基线是不重复计算的实现（代价是广度优先的内存）。

## 结果

Racket 9.3 CS，Linux x86_64。书中例子为 TRS2 第 1 至 9 章与 arith。

| 版本 | 书中例子 | 套件耗时 | `very-recursiveo` 10⁶ 个答案 | 逃逸反例（期望 `(1)` / `(1 2)`） |
| --- | --- | ---: | ---: | --- |
| 基线 | 285 passed，其中 15 项放宽为只比较答案集合 | 464 s | 64.3 s | 不适用 |
| `trs2-local` | 284 passed，严格；跳过 ch6 第 25 帧 | 574 s | 超线性 | `()` / `(2)`，丢答案 |
| `trs2-exn` | 284 passed，严格；跳过 ch6 第 25 帧 | 794 s | 超线性 | `()` / `(2)`，丢答案 |
| `trs2-shift` | 284 passed，严格；跳过 ch6 第 25 帧 | 20 s | 超线性 | 不适用（不用 amb） |
| `trs2-local-fused` | 285 passed，严格 | 41 s | 5.2 s | `()` / `(2)`，丢答案 |
| `trs2-amb-plain` | 285 passed，严格 | 55 s | 3.0 s | `(1)` / `(1 2)` |
| `trs2-amb-pause` | 285 passed，严格 | 55 s | 4.8 s | `(1)` / `(1 2)` |
| `trs2-empty-values` | 285 passed，严格 | 51 s | 4.8 s | `()` / `(2)`，丢答案 |
| `trs2-amb-output` | 285 passed，严格 | 54 s | 4.0 s | `()` / `(2)`，丢答案 |
| `trs2-amb-empty` | 285 passed，严格 | 57 s | 5.4 s | `(1)` / `(1 2)` |

耗时都是单次运行，仅供参考。

`trs2-shift` 的 `conj2` 是函数复合，会抹掉合取的括号（[#68](https://github.com/NoahStoryM/algebraic-control-flow-math/issues/68)），所以每个语法上的 `conj`（含 `fresh` 体、关系体）都 reify 自己的体。只划关系体时 ch8:205 换位；只划非空 `fresh` 时书中例子全过，但 `laws.out` 中 `(fresh () p f)` 的结合律例子与原书不同。

**读表时要注意（E5）。** 融合后，`disj2` / `conj2` / `ifte` / `once` / `defrel` 都直接在 TRS2 流上运行，书中例子从不调用 `suspend`（计数为 0）。所以五个融合版的 285 项只验证了融合后的流组合子，**没有**验证各自的暂停机制。暂停机制由下面几项检验：

- 两个未融合版（`trs2-local`、`trs2-exn`）：每次 `defrel` 暂停都经过各自的机制；
- `compare-typed.rkt`：13 个对照程序（含公平性反例 C1 到 C7），在 `trs2-exn` 上与原书全同；无类型内核上的同一组程序见 `compare.rkt`；
- 逃逸反例：一个没有配对的普通 `in-amb/do` 包住会暂停的目标；
- `trs2-amb-plain-limits.rkt`：不经过暂停效应的代价，即手写目标中无限暂停的子目标会饿死兄弟分支。

## 结论（截至 11/9）

- 只用 amb master，有两种做法。
  - **暂停作为效应**（`trs2-local` 用单独的 prompt tag，`trs2-exn` 用异常）：暂停级公平性完整保留，但每个 `in-amb` 都必须配对一个暂停 handler。这条纪律只能由 ambKanren 的 `reify-goal` 守住，Goal 内部一个未配对的 `in-amb/do` 就会丢答案。
  - **不让 amb 看到暂停**（`trs2-amb-plain`）：不会逃逸，但手写的直接风格目标内部会退化为答案级调度。
- 扩展 amb 的四种接口中，只有把暂停 handler 并进 amb 自己的驱动器的两种（`pause` / `in-amb/pause`，`amb-empty` / `#:empty-values`）能由构造保证配对，不会逃逸。另外两种（`current-amb-empty-values`、`amb-output`）把暂停当作普通输出，未配对的消费者照样丢答案。
- 数学上的讨论（`Stream (1 + A)`、tick、handler 的嵌套）见数学仓库 [#68](https://github.com/NoahStoryM/algebraic-control-flow-math/issues/68)。
