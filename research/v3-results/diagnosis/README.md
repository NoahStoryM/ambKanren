# 性能与顺序诊断的脚本

结论见 issue #18。这些文件只是复现诊断用的，不属于包本身。

- `measure.rkt`：在当前进程中 `dynamic-require` 一个已编译模块，打印 CPU、real、GC 毫秒数，以及累计分配量（MB）。

  ```sh
  racket measure.rkt '(file "miniKanren/amb/trs2/tests/main.rkt")'
  racket measure.rkt '(submod (file "tools/native-trs2.rkt") test)'
  ```

- `micro.rkt`：测量 amb 各种操作每次迭代的分配字节数和耗时。
- `shift-variants.rkt`：比较 `shift-at`、`shift0-at`、`control0-at` 和普通 abort 的开销，并检查 body 是否仍在 prompt 之内（结论见 issue #18 的更正评论）。
- `amb-core-ablation.patch`：打在 amb `81bf0c9` 的 `pkgs/amb/private/core.rkt` 上。设置环境变量 `AMB_FAST` 时，`amb-generate*`、`make-amb`、`amb-fail`、`amb-pause` 改为只捕获一次续延、使用普通 abort，不再走 `racket/control` 的 `shift-at`/`reset-at`；不设置时行为不变。它相当于 `control0-at` 配 `reset-at`，语义与 `shift-at` 相同。这只是实验，没有提交到 amb。
- `amb-core-counters.patch`：打在上一个补丁之后。设置 `AMB_COUNT` 时，进程退出前打印局部搜索、选择、续延恢复、暂停、失败的次数。只统计原实现的路径，所以不要同时设置 `AMB_FAST`。

打补丁以后，要重新编译 amb、typed-amb 和 ambKanren，再运行测试模块。
