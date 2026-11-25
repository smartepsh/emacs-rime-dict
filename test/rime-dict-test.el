;;; rime-dict-test.el --- Tests for personal dictionaries -*- lexical-binding: t; -*-

(require 'ert)
(require 'rime-dict)

(defun rime-dict-test--put (name content)
  (let ((file (expand-file-name name rime-dict-user-data-dir))
        (coding-system-for-write 'utf-8-unix))
    (make-directory (file-name-directory file) t)
    (write-region content nil file nil 'silent)))

(defun rime-dict-test--get (name)
  (with-temp-buffer
    (insert-file-contents (expand-file-name name rime-dict-user-data-dir))
    (buffer-string)))

(defconst rime-dict-test--schema
  "engine:\n  translators: [script_translator, table_translator@custom_phrase]\ncustom_phrase:\n  db_class: stabledb\n  user_dict: shortcuts\ntranslator:\n  dictionary: base\n  packs: [personal]\n")

(defmacro rime-dict-test--with-data (&rest body)
  (declare (indent 0))
  `(let* ((temp (make-temp-file "rime-dict-test-" t))
          (rime-dict-user-data-dir (file-name-as-directory temp))
          (rime-dict-shared-data-dir nil)
          (rime-dict-auto-reload nil)
          (rime-dict-default-schema "demo")
          (rime-dict-current-schema-function nil)
          (rime-dict--pending-reload nil)
          (rime-dict-targets '(("demo" :word "personal.dict.yaml" :shortcut auto))))
     (unwind-protect
         (progn
           (rime-dict-test--put "build/default.yaml" "schema_list: [{schema: demo}]\n")
           (rime-dict-test--put "build/demo.schema.yaml" rime-dict-test--schema)
           (rime-dict-test--put "base.dict.yaml"
                                "---\nname: base\nimport_tables: [words, counts]\n...\n")
           (rime-dict-test--put "words.dict.yaml"
                                "---\nname: words\n...\n输入法\tshu ru fa\t120\n输入法设置\tshu ru fa she zhi\t1\n")
           (rime-dict-test--put "counts.dict.yaml"
                                "---\nname: counts\ncolumns: [weight, text]\n...\n3\t输入法\n")
           (rime-dict-test--put "shortcuts.txt" "# Rime table\n输入法\tsrf\t2\n")
           ,@body)
       (delete-directory temp t))))

(defun rime-dict-test--on-region (command word)
  (with-temp-buffer
    (insert word)
    (goto-char (point-min))
    (push-mark (point-max) t t)
    (let ((transient-mark-mode t)
          (mark-active t))
      (call-interactively command))))

(defun rime-dict-test--invoke (command word code &optional weight)
  "Exercise COMMAND with a selected WORD and simulated code/weight input."
  (cl-letf (((symbol-function 'completing-read)
             (lambda (&rest _)
               (when weight (setq rime-dict--want-weight t))
               code))
            ((symbol-function 'read-string)
             (lambda (&rest _) (or weight (ert-fail "Unexpected prompt")))))
    (rime-dict-test--on-region command word)))

(ert-deftest rime-dict-lookup-respects-word-columns-and-sources ()
  (rime-dict-test--with-data
    (let* ((cat (rime-dict--catalog-for "demo" rime-dict-user-data-dir))
           (target (rime-dict--target "demo" :word cat rime-dict-user-data-dir))
           (hits (rime-dict--lookup "输入法" cat target :word)))
      (should (equal (rime-dict--current-schema rime-dict-user-data-dir) "demo"))
      (should (equal (file-name-nondirectory (rime-dict--catalog-shortcut cat))
                     "shortcuts.txt"))
      (should (= 3 (length hits)))
      (should (member '(:word "shu ru fa" "120")
                      (mapcar (lambda (hit) (list (rime-dict--hit-role hit)
                                                  (rime-dict--hit-code hit)
                                                  (rime-dict--hit-weight hit))) hits)))
      (should (cl-find-if (lambda (hit) (and (null (rime-dict--hit-code hit))
                                             (equal (rime-dict--hit-weight hit) "3"))) hits))
      (should (cl-find-if (lambda (hit) (eq :shortcut (rime-dict--hit-role hit))) hits)))))

(ert-deftest rime-dict-word-upserts-weight-and-deploys-once-per-change ()
  (rime-dict-test--with-data
    (let ((calls 0) (rime-dict-auto-reload t)
          (original (rime-dict-test--get "words.dict.yaml")))
      (cl-letf (((symbol-function 'rime-deploy) (lambda () (cl-incf calls))))
        (rime-dict-test--invoke 'rime-dict-add-word "输入法" "shu ru fa" "500")
        (rime-dict-test--invoke 'rime-dict-add-word "输入法" "shu ru fa")
        (rime-dict-test--invoke 'rime-dict-add-word "输入法" "shu ru fa" "500")
        (should (= calls 1))
        (rime-dict-test--invoke 'rime-dict-add-word "输入法" "shu ru fa" "800")
        (should (= calls 2))
        (let ((content (rime-dict-test--get "personal.dict.yaml")))
          (should (string-suffix-p "输入法\tshu ru fa\t800\n" content))
          (should (= 1 (cl-count ?输 content))))
        (should (equal original (rime-dict-test--get "words.dict.yaml")))))))

(ert-deftest rime-dict-shortcut-keeps-alternatives-and-honors-disabled-reload ()
  (rime-dict-test--with-data
    (cl-letf (((symbol-function 'rime-deploy) (lambda () (ert-fail "Reload disabled"))))
      (rime-dict-test--invoke 'rime-dict-add-shortcut "输入法" "ime")
      (rime-dict-test--invoke 'rime-dict-add-shortcut "输入法" "ime")
      (rime-dict-test--invoke 'rime-dict-add-shortcut "输入法" "srf" "6")
      (rime-dict-test--invoke 'rime-dict-add-shortcut "算法" "srf")
      (should (equal (rime-dict-test--get "shortcuts.txt")
                     "# Rime table\n输入法\tsrf\t6\n输入法\time\n算法\tsrf\n")))))

(ert-deftest rime-dict-prompts-for-word-and-prefills-one-same-kind-code ()
  (rime-dict-test--with-data
    (with-temp-buffer
      (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "输入法"))
                ((symbol-function 'completing-read)
                 (lambda (prompt codes &optional _pred _match initial &rest _args)
                   (should (string-match-p "counts.dict.yaml" prompt))
                   (should (string-match-p "120" prompt))
                   (should (equal codes '("shu ru fa")))
                   (should (equal initial "shu ru fa"))
                   initial)))
        (call-interactively #'rime-dict-add-word)))))

(ert-deftest rime-dict-rime-ice-pack-requires-base-syllables-before-saving ()
  (rime-dict-test--with-data
    (rime-dict-test--put "build/demo.schema.yaml"
                         (replace-regexp-in-string "dictionary: base" "dictionary: rime_ice"
                                                   rime-dict-test--schema))
    (rime-dict-test--put "rime_ice.dict.yaml"
                         "---\ncolumns: [code, text]\nimport_tables: [words, counts]\n...\ntuo\t拓\n")
    (rime-dict-test--put "words.dict.yaml"
                         "---\ncolumns: [text, weight, code]\n...\ntovu\t2\twen zhu\n竹\t1\tzhu\n#注释\t0\ttovu\n")
    (rime-dict-test--put "counts.dict.yaml" "---\ncolumns: [text, weight]\n...\ntovu\t3\n")
    (let ((prior "---\nname: personal\n...\n旧误码\ttovu\n")
          (rime-dict-auto-reload t) (deploys 0))
      (rime-dict-test--put "personal.dict.yaml" prior)
      (cl-letf (((symbol-function 'rime-deploy) (lambda () (cl-incf deploys))))
        (let ((err (should-error (rime-dict-test--invoke 'rime-dict-add-word "拓竹" "tovu")
                                 :type 'user-error)))
          (should (string-match-p "没有音节 tovu" (error-message-string err))))
        (should (zerop deploys))
        (should (equal prior (rime-dict-test--get "personal.dict.yaml")))
        (rime-dict-test--invoke 'rime-dict-add-word "拓竹" "tuo zhu")
        (should (string-suffix-p "拓竹\ttuo zhu\n" (rime-dict-test--get "personal.dict.yaml")))
        (rime-dict-test--invoke 'rime-dict-add-shortcut "拓竹" "tovu")
        (should (string-suffix-p "拓竹\ttovu\n" (rime-dict-test--get "shortcuts.txt")))
        (should (= deploys 2)))
      (setq rime-dict-targets '(("demo" :word "new.dict.yaml" :pack t)))
      (should-error (rime-dict-test--invoke 'rime-dict-add-word "拓竹" "tovu") :type 'user-error)
      (dolist (name '("new.dict.yaml" "demo.custom.yaml"))
        (should-not (file-exists-p (expand-file-name name rime-dict-user-data-dir)))))))

(ert-deftest rime-dict-rime-ice-partial-sources-and-shape-tables ()
  (rime-dict-test--with-data
    (rime-dict-test--invoke 'rime-dict-add-word "形码词" "tovu")
    (rime-dict-test--put "build/demo.schema.yaml"
                         (replace-regexp-in-string "dictionary: base" "dictionary: rime_ice"
                                                   rime-dict-test--schema))
    (rime-dict-test--put "rime_ice.dict.yaml" "---\nimport_tables: [only_in_binary]\n...\n")
    (rime-dict-test--invoke 'rime-dict-add-word "新词" "xin ci")
    (should (string-suffix-p "形码词\ttovu\n新词\txin ci\n"
                             (rime-dict-test--get "personal.dict.yaml")))))

(defun rime-dict-test--install-pinyin-tables ()
  (rime-dict-test--put "build/demo.schema.yaml"
                       (replace-regexp-in-string "dictionary: base" "dictionary: rime_ice"
                                                 rime-dict-test--schema))
  (rime-dict-test--put "rime_ice.dict.yaml"
                       "---\nimport_tables: [chars, words, counts]\n...\n")
  (rime-dict-test--put "chars.dict.yaml"
                       (concat "---\ncolumns: [code, text]\n...\nzhong\t重\nchong\t重\nqing\t庆\n"
                               "ta\t拓\ntuo\t拓\nzhu\t竹\nwu\t雾\nmi\t蜜\nda\t大\n"))
  (rime-dict-test--put "words.dict.yaml" "---\nname: words\n...\n重庆\tchong qing\n"))

(ert-deftest rime-dict-infers-pinyin-from-a-known-phrase-and-characters ()
  (rime-dict-test--with-data
    (rime-dict-test--install-pinyin-tables)
    (rime-dict-test--put "counts.dict.yaml" "---\ncolumns: [weight, text]\n...\n8\t重庆雾蜜\n")
    (rime-dict-test--put "shortcuts.txt" "重庆雾蜜\tcqwm\n")
    (let ((prompts 0) (reloads 0) (rime-dict-auto-reload t))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (prompt choices _predicate _match initial &rest _)
                   (cl-incf prompts)
                   (should (string-match-p "已补全全拼" prompt))
                   (should (equal choices '("chong qing wu mi")))
                   (should (equal initial (car choices)))
                   (setq rime-dict--want-weight t)
                   initial))
                ((symbol-function 'read-string)
                 (lambda (prompt &rest _) (should (string-match-p "权重" prompt)) "320"))
                ((symbol-function 'rime-deploy) (lambda () (cl-incf reloads))))
        (rime-dict-test--on-region 'rime-dict-add-word "重庆雾蜜"))
      (should (= prompts 1)) (should (= reloads 1))
      (should (string-suffix-p "重庆雾蜜\tchong qing wu mi\t320\n"
                               (rime-dict-test--get "personal.dict.yaml"))))))

(ert-deftest rime-dict-asks-only-for-an-unresolved-reading-and-reuses-the-saved-word ()
  (rime-dict-test--with-data
    (rime-dict-test--install-pinyin-tables)
    (let ((asked 0))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (prompt choices _predicate match &optional initial &rest _)
                   (if match
                       (progn
                         (should (string-match-p "「拓」" prompt))
                         (should (equal choices '("ta" "tuo")))
                         (cl-incf asked)
                         "tuo")
                     (should (equal initial "tuo zhu")) initial))))
        (rime-dict-test--on-region 'rime-dict-add-word "拓竹")
        (cl-letf (((symbol-function 'rime-dict--infer-pinyin)
                   (lambda (&rest _) (ert-fail "The whole word already has a reading"))))
          (rime-dict-test--on-region 'rime-dict-add-word "拓竹")))
      (should (= asked 1))
      (should (string-suffix-p "拓竹\ttuo zhu\n" (rime-dict-test--get "personal.dict.yaml"))))))

(ert-deftest rime-dict-asks-for-only-a-missing-piece-and-keeps-the-result-editable ()
  (rime-dict-test--with-data
    (rime-dict-test--install-pinyin-tables)
    (let ((asked 0))
      (cl-letf (((symbol-function 'read-string)
                 (lambda (prompt &rest _)
                   (should (string-match-p "「龘龘」缺少读音" prompt))
                   (cl-incf asked) "da da"))
                ((symbol-function 'completing-read)
                 (lambda (_prompt _choices _predicate _match initial &rest _)
                   (should (equal initial "wu da da mi"))
                   "wu da da wu")))
        (rime-dict-test--on-region 'rime-dict-add-word "雾龘龘蜜"))
      (should (= asked 1))
      (should (string-suffix-p "雾龘龘蜜\twu da da wu\n" (rime-dict-test--get "personal.dict.yaml"))))
    (rime-dict-test--invoke 'rime-dict-add-word "龘" "da")
    (should (string-suffix-p "龘\tda\n" (rime-dict-test--get "personal.dict.yaml")))))

(ert-deftest rime-dict-infers-for-imported-pinyin-targets-and-keeps-shortcuts-literal ()
  (rime-dict-test--with-data
    (rime-dict-test--install-pinyin-tables)
    (setq rime-dict-targets '(("demo" :word "words.dict.yaml" :shortcut auto)))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_prompt _choices _predicate _match initial &rest _)
                 (should (equal initial "wu mi")) initial)))
      (rime-dict-test--on-region 'rime-dict-add-word "雾蜜"))
    (should (string-suffix-p "雾蜜\twu mi\n" (rime-dict-test--get "words.dict.yaml")))
    (cl-letf (((symbol-function 'rime-dict--infer-pinyin)
               (lambda (&rest _) (ert-fail "Literal shortcuts have user-chosen keys"))))
      (rime-dict-test--invoke 'rime-dict-add-shortcut "拓竹" "tovu"))
    (should (string-suffix-p "拓竹\ttovu\n" (rime-dict-test--get "shortcuts.txt")))))

(ert-deftest rime-dict-target-without-weight-and-reordered-columns ()
  (rime-dict-test--with-data
    (rime-dict-test--put "personal.dict.yaml" "---\nname: personal\ncolumns: [code, text]\n...\n")
    (rime-dict-test--invoke 'rime-dict-add-word "公式" "gong shi")
    (let ((before (rime-dict-test--get "personal.dict.yaml")))
      (should (string-suffix-p "gong shi\t公式\n" before))
      (should-error (rime-dict-test--invoke 'rime-dict-add-word "公式" "gong shi" "42")
                    :type 'user-error)
      (should (equal before (rime-dict-test--get "personal.dict.yaml"))))
    (rime-dict-test--put "personal.dict.yaml"
                         "---\nname: personal\ncolumns: [code, weight, text, stem]\n...\nabc\t1\t工事\txyz")
    (rime-dict-test--invoke 'rime-dict-add-word "工事" "abc" "15")
    (should (string-suffix-p "abc\t15\t工事\txyz" (rime-dict-test--get "personal.dict.yaml")))))

(ert-deftest rime-dict-missing-rime-is-only-an-error-when-reload-is-enabled ()
  (rime-dict-test--with-data
    (let ((original (symbol-function 'require)))
      (cl-letf (((symbol-function 'rime-deploy) nil)
                ((symbol-function 'require)
                 (lambda (name &rest args)
                   (unless (eq name 'rime) (apply original name args)))))
        (let ((rime-dict-auto-reload t))
          (should-error (rime-dict-test--invoke 'rime-dict-add-shortcut "新词" "xc")
                        :type 'user-error))
        (should-not (string-match-p "新词" (rime-dict-test--get "shortcuts.txt")))
        (rime-dict-test--invoke 'rime-dict-add-shortcut "新词" "xc")
        (should (string-match-p "新词\txc\n" (rime-dict-test--get "shortcuts.txt")))))))

(ert-deftest rime-dict-default-reload-follows-package-availability ()
  (let ((form (car (get 'rime-dict-auto-reload 'standard-value))))
    (dolist (installed '(nil t))
      (cl-letf (((symbol-value 'features) (remq 'rime (copy-sequence features)))
                ((symbol-function 'locate-library)
                 (lambda (name &rest _) (and (equal name "rime") installed "rime.el"))))
        (should (eq (eval form t) installed))))))

(ert-deftest rime-dict-retry-failed-deployment-without-adding-a-second-row ()
  (rime-dict-test--with-data
    (let ((rime-dict-auto-reload t) (calls 0))
      (cl-letf (((symbol-function 'rime-deploy)
                 (lambda () (when (= (cl-incf calls) 1) (error "Unavailable")))))
        (should-error (rime-dict-test--invoke 'rime-dict-add-word "输入法" "shu ru fa")
                      :type 'user-error)
        (rime-dict-test--invoke 'rime-dict-add-word "输入法" "shu ru fa")
        (should (= calls 2))
        (should (= 1 (cl-count ?输 (rime-dict-test--get "personal.dict.yaml"))))))))

(ert-deftest rime-dict-first-use-mounts-personal-pack-idempotently ()
  (rime-dict-test--with-data
    (rime-dict-test--put "build/demo.schema.yaml"
                         (replace-regexp-in-string "  packs: \\[personal\\]\n" ""
                                                   rime-dict-test--schema))
    (setq rime-dict-targets '(("demo" :word "personal.dict.yaml" :pack t)))
    (rime-dict-test--invoke 'rime-dict-add-word "输入法" "shu ru fa")
    (let ((before (rime-dict-test--get "demo.custom.yaml")))
      (should (equal (gethash "translator/packs/+" (gethash "patch" (rime-dict--yaml before)))
                     '("personal")))
      (rime-dict-test--invoke 'rime-dict-add-word "输入法" "shu ru fa")
      (should (equal before (rime-dict-test--get "demo.custom.yaml"))))
    (rime-dict-test--put "demo.custom.yaml" "# keep\npatch:\n  menu/page_size: 7  # keep too\n")
    (should (rime-dict--mount-pack "demo" "personal" rime-dict-user-data-dir))
    (should (string-match-p "menu/page_size: 7  # keep too"
                            (rime-dict-test--get "demo.custom.yaml")))))

(ert-deftest rime-dict-schema-fallback-and-explicit-override ()
  (rime-dict-test--with-data
    (let ((rime-dict-targets '(("demo" :shortcut auto) ("shape" :word "shape.dict.yaml"))))
      (rime-dict-test--put "build/default.yaml" "schema_list: [{schema: demo}, {schema: shape}]\n")
      (should (equal "demo" (rime-dict--choose-schema rime-dict-user-data-dir nil)))
      (let ((rime-dict-current-schema-function (lambda () "shape")))
        (should (equal "shape" (rime-dict--choose-schema rime-dict-user-data-dir nil))))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt _choices _pred _match _input _history initial)
                   (should (equal "demo" initial)) "shape")))
        (should (equal "shape" (rime-dict--choose-schema rime-dict-user-data-dir t)))
        (let ((rime-dict-current-schema-function (lambda () "unmapped")))
          (should (equal "shape" (rime-dict--choose-schema rime-dict-user-data-dir nil))))))))

(ert-deftest rime-dict-mount-preserves-existing-packs-and-customizations ()
  (rime-dict-test--with-data
    (dolist (source '("patch: {menu/page_size: 7, translator/packs/+: [one, two]}\n"
                      "patch:\n  translator/packs/+:\n    - one\n    - two # keep\n  menu/page_size: 7\n"
                      "patch: {menu/page_size: 7, translator/packs: [one, two]}\n"
                      "patch:\n  {menu/page_size: 7, translator/packs: [one, two]}\n"))
      (rime-dict-test--put "demo.custom.yaml" source)
      (should (rime-dict--mount-pack "demo" "personal" rime-dict-user-data-dir))
      (let* ((saved (rime-dict-test--get "demo.custom.yaml"))
             (patch (gethash "patch" (rime-dict--yaml saved)))
             (packs (append (gethash "translator/packs" patch)
                            (gethash "translator/packs/+" patch))))
        (should (equal 7 (gethash "menu/page_size" patch)))
        (should (equal (sort packs #'string<) '("one" "personal" "two")))
        (when (string-match-p "# keep" source) (should (string-match-p "# keep" saved)))
        (should-not (rime-dict--mount-pack "demo" "personal" rime-dict-user-data-dir))))))

(ert-deftest rime-dict-literal-fields-and-comments ()
  (rime-dict-test--with-data
    (rime-dict-test--put "shortcuts.txt" "# Rime table\n#tag\tprevious-note\n")
    (rime-dict-test--invoke 'rime-dict-add-shortcut "#tag" "x.+" "3")
    (rime-dict-test--invoke 'rime-dict-add-shortcut "#tag" "x.+" "7")
    (should (equal (rime-dict-test--get "shortcuts.txt")
                   "# Rime table\n#tag\tprevious-note\n# no comment\n#tag\tx.+\t7\n"))
    (should-error (rime-dict-test--invoke 'rime-dict-add-word "A\tB" "abc") :type 'user-error)
    (should-error (rime-dict-test--invoke 'rime-dict-add-word "A+B" "a\nb") :type 'user-error)))

(ert-deftest rime-dict-atomic-writes-preserve-symlinks-and-detect-other-writers ()
  (rime-dict-test--with-data
    (let* ((file (expand-file-name "shortcuts.txt" rime-dict-user-data-dir))
           (actual (concat file ".original")))
      (rename-file file actual)
      (make-symbolic-link actual file)
      (rime-dict-test--invoke 'rime-dict-add-shortcut "词" "t")
      (should (file-symlink-p file))
      (should (string-suffix-p "词\tt\n" (rime-dict-test--get "shortcuts.txt.original")))
      (should-error (rime-dict--atomic-edit file
                                            (lambda ()
                                              (insert "interrupted edit")
                                              (write-region "another writer\n" nil actual nil 'silent)
                                              t))
                    :type 'user-error)
      (should (equal "another writer\n" (rime-dict-test--get "shortcuts.txt.original"))))))

;;; rime-dict-test.el ends here
