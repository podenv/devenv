(use-package neuron-mode
  :defer t
  :config
  ;; Use title name as file name
  (setq-default neuron-id-format (lambda (a) a)))
