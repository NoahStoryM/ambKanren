# 局部搜索组合器实验（ambKanren issue #14）

研究分支，不改动正式代码。结论与数据见 issue #14 的对应评论；本文件只说明文件和复现方式。

## 环境

- Racket v9.3 [cs]（`NoahStoryM/racket-dist` release v9.3，`racket-9.3-x86_64-linux-buster-cs.sh`），Linux x86_64
- amb：`research/observable-pause-20261109`（基于 master `186bd1f`，只新增 `pause` / `amb-paused` / `in-amb/pause`；不含 `amb-tree.info`）。除 `trs2-amb-pause*.rkt`、`pause-escape-amb.rkt` 外，其余文件在 amb master 上也能运行
- ambKanren：本分支，基于 master `ad2586d`
- miniKanren-typed：master `86c28eb`（参照实现与书中测试）
- typed-data-queue：master `07e8cbd`（ambKanren TRS1 的依赖）

安装：在各仓库根目录 `raco pkg install --batch --deps force --link <dir>`（沙箱无法访问 pkgs.racket-lang.org，所以手动链接）。

## 文件

| 文件 | 内容 |
|---|---|
| `kernel.rkt` | 无类型 TRS2 风格 miniKanren 内核，`Goal : S → S`，组合器可替换 |
| `schemes.rkt` | 四种调度：`global`（现状）、`answers`（实验 B）、`pause/disj`、`pause/full`（实验 D） |
| `programs.rktl` | 13 个对照程序（有限、C1–C7 反例、递归关系），同一份文本供有类型/无类型使用 |
| `reference-trs2.rkt` → `reference-trs2.out` | 用 miniKanren-typed TRS2（原书流实现）求参照答案 |
| `compare.rkt` → `compare.out` | 四种调度 × 13 个程序，与参照逐个比较（实验 C） |
| `traces.rkt` → `traces.out` | Spawn / Yield / Pause / Done 执行轨迹 |
| `trs2-local.rkt` + `-tests.rkt` | 有类型 TRS2：`disj2/conj2/ifte/once` = 原书流组合器经 reify/reflect 局部提升；书中例子**全部严格比较** |
| `trs2-local-fused.rkt` + `-tests.rkt` | 同上，加融合律 `reify ∘ lift = id`、`defrel` 提升为 TRS2 自身的悬挂 |
| `trs2-amb-pause.rkt` + `-tests.rkt` | 同上，但悬挂用 amb 分支新增的 `pause` / `in-amb/pause`，无额外 prompt tag |
| `trs2-empty-values.rkt` + `-tests.rkt` | 同上，悬挂为 `current-amb-empty-values` 下的空 `(amb)`，局部搜索用普通 `in-amb/do` 识别 `paused` 叶子；需要 amb `research/amb-empty-values-20261109` |
| `trs2-exn.rkt` + `-tests.rkt` / `compare-typed.rkt` / `pause-escape-exn.rkt` | **amb + raise/with-handlers**（amb master）：未融合版，暂停是 `(when (amb #t #f) (raise PAUSE #t))`，handler 装在每个局部搜索之内；书中 284 项严格通过（跳过 ch6 第 25 帧），13 个对照程序全同；未配对的普通 `in-amb/do` 仍会丢答案 |
| `trs2-amb-plain.rkt` + `-tests.rkt` / `-limits.rkt` | **只需 amb master**：组合子为融合后的 TRS2 流，amb 只运行直接风格的 Goal；`==`/`succeed`/`fail` 产生成熟流，手写 Goal 惰性 reify；`reflect` 强制悬挂（手写 Goal 内部为答案级调度，`-limits.rkt` 记录边界） |
| `trs2-amb-empty.rkt` + `-tests.rkt` | 悬挂为 `(when (amb #t #f) (amb-empty))`，局部搜索为 `(in-amb/do (g s) #:empty-values (list PAUSED))`，无需 `ann`；需要 amb `research/amb-empty-20261109` |
| `trs2-amb-output.rkt` + `-tests.rkt` | 早期方案：悬挂为 `(when (amb #t #f) (amb-output PAUSED))`，局部搜索用普通 `in-amb/do` 识别 `paused` 叶子；需要 amb `research/amb-output-20261109` |
| `pause-escape*.rkt` | 额外 prompt tag 方案的反例，以及在 amb `pause` 下的复测 |
| `trs1-no-pause.rkt` | TRS1 中 `condi` 左支 `nevero` 发散（TRS1 没有可观察的暂停边界） |
| `trs1-condi-order.rkt` | 三分支 `condi` 顺序：原书 TRS1 与 ambKanren TRS1 一致 |
| `gen-*.rkt` | 由 `trs2/main.rkt` 生成上述三个有类型变体的脚本 |

## 复现

```sh
cd research/local-search
racket reference-trs2.rkt > reference-trs2.out
racket compare.rkt
racket traces.rkt
raco test trs2-local-tests.rkt          # 约 9.5 分钟，跳过 ch6 第 25 帧
raco test trs2-local-fused-tests.rkt    # 约 41 秒
raco test trs2-amb-pause-tests.rkt      # 约 55 秒，需要 amb research/observable-pause-20261109
raco test trs2-empty-values-tests.rkt   # 约 51 秒，需要 amb research/amb-empty-values-20261109
raco test trs2-amb-output-tests.rkt     # 约 54 秒，需要 amb research/amb-output-20261109
raco test trs2-amb-empty-tests.rkt      # 约 57 秒，需要 amb research/amb-empty-20261109
raco test trs2-amb-plain-tests.rkt      # 约 55 秒，amb master
racket trs2-amb-plain-limits.rkt
raco test trs2-exn-tests.rkt            # 约 13 分钟，amb master
racket compare-typed.rkt
racket pause-escape-exn.rkt
racket pause-escape.rkt
racket pause-escape-amb.rkt            # observable-pause 分支
racket pause-escape-empty-values.rkt   # amb-empty-values 分支
racket pause-escape-amb-output.rkt     # amb-output 分支
racket pause-escape-amb-empty.rkt      # amb-empty 分支：(1) 与 (1 2)
```

目录内 `info.rkt` 把整个目录排除在 `raco setup` 与包测试之外。

实验 A（`amb-tree.info` 传播）的文件已删除：后续实验表明 `info` 不是必要条件，amb 分支也已去掉该扩展。结果仍记录在 issue #14 的评论中。
