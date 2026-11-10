;; Two finite examples from Astra (algebraic-control-flow-math #68).
(defrel (oneo q) (== q 'A))
(defrel (late-failo) fail)
(printf "  once:  ~s\n" (run* q (disj (once (oneo q)) (== q 'B))))
(printf "  ifte:  ~s\n" (run* q (ifte (late-failo) succeed (== q 'ok))))
