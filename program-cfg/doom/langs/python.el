;;; ../home-manager/program-cfg/doom/langs/python.el -*- lexical-binding: t; -*-

(if (featurep 'yf-feat-python)
    (set-formatter! 'ruff
      '("ruff"
        "format"
        "--stdin-filename" (or buffer-file-name "")
        "-")
      :modes '(python-mode)))
