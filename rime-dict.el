;;; rime-dict.el --- Add words and shortcuts to Rime dictionaries -*- lexical-binding: t; -*-

;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (yaml "1.2.4"))
;; Keywords: convenience, input method

;;; Commentary:

;; Use M-x rime-dict-add-word or M-x rime-dict-add-shortcut on a region,
;; or enter a word in the minibuffer.  In the code prompt, RET saves and
;; C-c C-w accepts the code and asks for an optional weight.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'json)
(require 'yaml)

(defvar rime-user-data-dir)
(defvar rime-share-data-dir)
(defvar rime--lib-loaded)
(declare-function rime-deploy "rime")
(declare-function rime-lib-get-schema-list "ext:librime-emacs")

(defgroup rime-dict nil
  "Edit personal Rime dictionaries from the minibuffer."
  :group 'leim)

(defcustom rime-dict-auto-reload
  (and (or (featurep 'rime) (locate-library "rime")) t)
  "When non-nil, call `rime-deploy' after saving changes.
Default to t when emacs-rime is installed.  With nil, the commands
also work in an Emacs without emacs-rime."
  :type 'boolean)

(defcustom rime-dict-user-data-dir nil
  "Rime user directory.  Nil uses `rime-user-data-dir' from emacs-rime.
Set this explicitly when using another frontend."
  :type '(choice (const :tag "Use emacs-rime" nil) directory))

(defcustom rime-dict-shared-data-dir nil
  "Optional Rime shared directory for sources supplied by a frontend.
Nil uses `rime-share-data-dir' when available."
  :type '(choice (const :tag "Use emacs-rime" nil) directory))

(defcustom rime-dict-default-schema "double_pinyin"
  "Schema ID to use when the current session cannot report one."
  :type 'string)

(defcustom rime-dict-targets
  '(("double_pinyin" :word "personal_pinyin.dict.yaml" :pack t :shortcut auto))
  "Map schema IDs to personal files, relative to the Rime user directory.
:word is a dictionary file.  With :pack t, mount it in translator/packs
on first use if needed.  Otherwise it must already be referenced by Rime.
:shortcut is a stabledb text file, or `auto' to use custom_phrase/user_dict.
Either target may be omitted."
  :type '(alist :key-type string :value-type plist))

(defcustom rime-dict-current-schema-function nil
  "Optional function returning the live schema ID, or nil if unknown.
Without it, a single enabled schema can be identified unambiguously."
  :type '(choice (const :tag "Use the enabled schema list" nil) function))

(cl-defstruct rime-dict--catalog sources shortcut namespace pinyin-sources pinyin-packs)
(cl-defstruct rime-dict--hit file role code weight)

(defvar rime-dict--word-history nil)
(defvar rime-dict--code-history nil)
(defvar rime-dict--want-weight nil)
(defvar rime-dict--pending-reload nil
  "User directories with a saved edit whose requested deployment failed.")

(defun rime-dict--user-dir ()
  (file-name-as-directory
   (expand-file-name
    (or rime-dict-user-data-dir
        (bound-and-true-p rime-user-data-dir)
        (progn (require 'rime nil t) (bound-and-true-p rime-user-data-dir))
        (user-error "请先设置 rime-dict-user-data-dir 指向 Rime 用户目录")))))

(defun rime-dict--locate (relative dir)
  "Resolve RELATIVE in DIR, then in the shared Rime directory."
  (let* ((local (expand-file-name relative dir))
         (shared (or rime-dict-shared-data-dir
                     (bound-and-true-p rime-share-data-dir)))
         (other (and shared (expand-file-name relative shared))))
    (if (and (not (file-exists-p local)) other (file-exists-p other))
        other local)))

(defun rime-dict--yaml (text)
  (yaml-parse-string text :object-key-type 'string :sequence-type 'list
                     :null-object nil :false-object nil))

(defun rime-dict--section (key)
  "Read one top-level KEY in a deployed Rime YAML buffer.
Rime emits block mappings.  Isolating sections avoids parsing unrelated,
potentially very large punctuation tables or spelling rules."
  (goto-char (point-min))
  (let ((name (regexp-quote key)) (case-fold-search nil))
    (when (re-search-forward
           (concat "^\\(?:" name "\\|\"" name "\"\\|'" name "'\\):") nil t)
      (let ((start (line-beginning-position)))
        (forward-line 1)
        (let ((end (if (re-search-forward "^[^ \t\r\n#]" nil t)
                       (line-beginning-position) (point-max))))
          (gethash key (rime-dict--yaml
                        (buffer-substring-no-properties start end))))))))

(defun rime-dict--current-schema (dir)
  (or (and rime-dict-current-schema-function
           (funcall rime-dict-current-schema-function))
      (let ((schemas (and (bound-and-true-p rime--lib-loaded)
                          (fboundp 'rime-lib-get-schema-list)
                          (rime-lib-get-schema-list))))
        (if schemas
            (and (null (cdr schemas)) (caar schemas))
          (let ((default (rime-dict--locate "build/default.yaml" dir)))
            (when (file-readable-p default)
              (with-temp-buffer
                (insert-file-contents default)
                (let ((list (rime-dict--section "schema_list")))
                  (and list (null (cdr list)) (gethash "schema" (car list)))))))))))

(defun rime-dict--choose-schema (dir manual)
  (let* ((known (rime-dict--current-schema dir))
         (names (mapcar #'car rime-dict-targets))
         (default (or (and (member rime-dict-default-schema names)
                           rime-dict-default-schema)
                      (car names))))
    (unless names (user-error "请先配置 rime-dict-targets"))
    (cond ((or manual (and known (not (member known names))))
           (completing-read
            (if (and known (not (member known names)))
                (format "方案 %s 没有映射，写入方案 › " known) "写入方案 › ")
            names nil t nil nil default))
          (known known)
          ((member rime-dict-default-schema names) rime-dict-default-schema)
          (t (user-error "默认方案 %s 没有目标映射" rime-dict-default-schema)))))

(defun rime-dict--header ()
  "Read the dictionary header, leaving point at the first data row."
  (goto-char (point-min))
  (let ((case-fold-search nil))
    (unless (re-search-forward "^\\.\\.\\.[ \t]*\\(?:#.*\\)?\r?$" nil t)
      (user-error "Rime 词典文件头缺少结束标记 ..."))
    (let* ((end (line-beginning-position))
           (start (save-excursion
                    (if (re-search-backward "^---[ \t]*\\(?:#.*\\)?\r?$" nil t)
                        (line-beginning-position 2) (point-min)))))
      (forward-line 1)
      (let ((text (buffer-substring-no-properties start end)))
        (rime-dict--yaml (if enable-multibyte-characters text
                           (decode-coding-string text 'utf-8-unix)))))))

(defun rime-dict--columns (file)
  "Return column names for FILE in the current buffer and skip its header."
  (goto-char (point-min))
  (if (string-suffix-p ".dict.yaml" file)
      (gethash "columns" (rime-dict--header) '("text" "code" "weight"))
    '("text" "code" "weight")))

(defun rime-dict--insert-table (file)
  "Read FILE into an empty unibyte buffer and return its columns.
Leave point at the data.  Decode only the header and matching rows."
  (set-buffer-multibyte nil)
  (insert-file-contents-literally file)
  (when (looking-at-p "\357\273\277") (delete-region 1 4))
  (rime-dict--columns file))

(defun rime-dict--dict-sources (id role dir)
  "Return the source file and its directly imported tables, as librime does."
  (let* ((file (rime-dict--locate (concat id ".dict.yaml") dir))
         (imports (when (file-readable-p file)
                    (with-temp-buffer
                      (insert-file-contents file)
                      (gethash "import_tables" (rime-dict--header))))))
    (cons (cons file role)
          (mapcar (lambda (name)
                    (cons (rime-dict--locate (concat name ".dict.yaml") dir)
                          role))
                  imports))))

(defun rime-dict--catalog-for (schema dir)
  "Find the static dictionaries and text tables used by deployed SCHEMA."
  (let ((file (rime-dict--locate (concat "build/" schema ".schema.yaml") dir))
        (seen (make-hash-table :test #'equal)) sources shortcut namespace
        pinyin-sources pinyin-packs)
    (unless (file-readable-p file)
      (user-error "找不到已部署的方案 %s，请先在 Rime 中部署" schema))
    (with-temp-buffer
      (insert-file-contents file)
      (let* ((engine (rime-dict--section "engine"))
             (translators
              (cl-loop for spec in (and engine (gethash "translators" engine))
                       when (string-match
                             "\\`\\(?:script\\|table\\)_translator\\(?:@\\(.+\\)\\)?\\'" spec)
                       collect (let ((ns (or (match-string 1 spec) "translator")))
                                 (cons ns (rime-dict--section ns)))))
             (main (or (assoc "translator" translators)
                       (cl-find-if (lambda (entry)
                                     (and (cdr entry)
                                          (not (string-empty-p
                                                (or (gethash "dictionary" (cdr entry)) "")))))
                                   translators))))
        (setq namespace (car main))
        (dolist (entry (if main (cons main (delq main translators)) translators))
          (pcase-let* ((`(,ns . ,options) entry)
                       (role (if (equal ns namespace) :word :other))
                       (dict (and options (gethash "dictionary" options)))
                       (extras (and options (gethash "packs" options)))
                       (class (and options (gethash "db_class" options)))
                       (user (and options (gethash "user_dict" options))))
            (dolist (id (cons dict extras))
              (when (and (stringp id) (not (string-empty-p id))
                         (not (gethash (cons id role) seen)))
                (puthash (cons id role) t seen)
                (let ((tables (rime-dict--dict-sources id role dir)))
                  (setq sources (append sources tables))
                  (when (and (eq role :word) (equal dict "rime_ice"))
                    (if (equal id dict)
                        (setq pinyin-sources (mapcar #'car tables))
                      (setq pinyin-packs (append (mapcar #'car tables) pinyin-packs)))))))
            (when (and (member class '("stabledb" "tabledb"))
                       (stringp user) (not (string-empty-p user)))
              (let ((table (expand-file-name (concat user ".txt") dir)))
                (when (and (equal ns "custom_phrase") (equal class "stabledb"))
                  (setq shortcut table))
                (push (cons table (cond ((equal class "tabledb") :mutable)
                                        ((equal ns "custom_phrase") :shortcut)
                                        (t :other)))
                      sources)))))))
    (make-rime-dict--catalog :sources (delete-dups sources) :shortcut shortcut
                             :namespace namespace :pinyin-sources pinyin-sources
                             :pinyin-packs pinyin-packs)))

(defun rime-dict--target (schema kind catalog dir)
  (let* ((mapping (cdr (assoc schema rime-dict-targets)))
         (value (plist-get mapping kind))
         (file (cond ((stringp value) (expand-file-name value dir))
                     ((and (eq kind :shortcut) (eq value 'auto))
                      (rime-dict--catalog-shortcut catalog)))))
    (unless file
      (user-error "方案 %s 未配置%s目标" schema
                  (if (eq kind :word) "普通词" "快捷简写")))
    (unless (string-suffix-p (if (eq kind :word) ".dict.yaml" ".txt") file)
      (user-error "目标文件类型与命令不符：%s" file))
    (when (eq (cdr (rime-dict--mounted-source file catalog)) :mutable)
      (user-error "此短语表由运行中的 Rime 写入，请将个人短语目标挂为 stabledb"))
    file))

(defun rime-dict--mounted-source (file catalog)
  (let ((sources (rime-dict--catalog-sources catalog)))
    (or (assoc file sources)
        (and (file-exists-p file)
             (cl-find-if (lambda (entry) (file-equal-p file (car entry))) sources)))))

(defun rime-dict--code-value (code kind)
  (when code
    (let ((trimmed (string-trim code)))
      (if (eq kind :word) (replace-regexp-in-string " +" " " trimmed) trimmed))))

(defun rime-dict--data-line-p (data-start)
  (or (not (eq (char-after (line-beginning-position)) ?#))
      (save-excursion (re-search-backward "^# no comment[ \t]*\r?$" data-start t))))

(defun rime-dict--matching-rows (text-regexp columns function)
  "Call FUNCTION with TSV fields for each text fully matching TEXT-REGEXP.
The regexp uses the current buffer's encoding.  Search the rows after point.
FUNCTION runs at the start of the matched line and may edit that line."
  (let ((idx (cl-position "text" columns :test #'equal))
        (data-start (point))
        (case-fold-search nil))
    (unless idx (user-error "词库缺少 text 列"))
    (let ((pattern (concat "^" (if (zerop idx) ""
                                 (format "\\(?:[^\t\n]*\t\\)\\{%d\\}" idx))
                           "\\(?:" text-regexp "\\)"
                           "\\(?:\t\\|\r?$\\)")))
      (while (re-search-forward pattern nil t)
        (beginning-of-line)
        (when (rime-dict--data-line-p data-start)
          (let ((row (buffer-substring-no-properties (point) (line-end-position))))
            (unless enable-multibyte-characters
              (setq row (decode-coding-string (string-remove-suffix "\r" row) 'utf-8-unix)))
            (funcall function (split-string row "\t" nil))))
        (forward-line 1)))))

(defun rime-dict--each-row (word columns function)
  "Call FUNCTION for each row with exactly WORD in its text column."
  (rime-dict--matching-rows
   (regexp-quote (if enable-multibyte-characters word (encode-coding-string word 'utf-8-unix)))
   columns function))

(defun rime-dict--lookup (word catalog target kind)
  (let ((seen (make-hash-table :test #'equal)) hits)
    (dolist (source (cons (cons target kind) (rime-dict--catalog-sources catalog)))
      (let* ((file (car source)) (path (file-truename file)))
        (when (and (file-readable-p file) (not (gethash path seen)))
          (puthash path t seen)
          (with-temp-buffer
            (let* ((columns (rime-dict--insert-table file))
                   (ci (cl-position "code" columns :test #'equal))
                   (wi (cl-position "weight" columns :test #'equal)))
              (rime-dict--each-row
               word columns
               (lambda (row)
                 (push (make-rime-dict--hit :file file :role (cdr source)
                                            :code (and ci (rime-dict--code-value
                                                           (nth ci row) (cdr source)))
                                            :weight (and wi (nth wi row)))
                       hits))))))))
    (nreverse hits)))

(defun rime-dict--field (value label)
  (when (or (not (stringp value)) (string-match-p "[\t\r\n\0]" value)
            (string-empty-p (string-trim value)))
    (user-error "%s应为一行非空文本，字段内不能含 Tab" label))
  (string-trim value))

(defun rime-dict--weight (input kind)
  (let ((value (string-trim input)))
    (unless (string-empty-p value)
      (unless (if (eq kind :shortcut)
                  (and (string-match-p "\\`[0-9]+\\'" value)
                       (<= (string-to-number value) 2147483647))
                (string-match-p "\\`[0-9]+\\(?:\\.[0-9]+\\)?\\'" value))
        (user-error "请输入非负%s权重" (if (eq kind :shortcut) "整数" "数值")))
      value)))

(defun rime-dict--accept-code-and-weight ()
  (interactive)
  (setq rime-dict--want-weight t)
  (exit-minibuffer))

(defvar rime-dict--code-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-w") #'rime-dict--accept-code-and-weight)
    (define-key map (kbd "SPC") #'self-insert-command)
    map))

(defun rime-dict--hit-label (hit)
  (let ((code (rime-dict--hit-code hit)) (weight (rime-dict--hit-weight hit)))
    (format "%s %s%s" (file-name-nondirectory (rime-dict--hit-file hit))
            (if (string-empty-p (or code "")) "未标注编码" code)
            (if (string-empty-p (or weight "")) "〔未标权重〕"
              (format "〔%s〕" weight)))))

(defun rime-dict--has-syllable-p (syllable column start)
  "Find SYLLABLE as a complete token in COLUMN of a UTF-8 table after START."
  (goto-char start)
  (let ((token (encode-coding-string syllable 'utf-8-unix))
        (case-fold-search nil))
    (catch 'found
      (while (search-forward token nil t)
        (let ((begin (- (point) (length token))))
          (when (and (memq (char-before begin) '(nil ?\n ?\t ?\s))
                     (memq (char-after) '(nil ?\n ?\r ?\t ?\s))
                     (= column (how-many "\t" (line-beginning-position) begin))
                     (rime-dict--data-line-p start))
            (throw 'found t)))))))

(defun rime-dict--check-pinyin (code sources)
  "Check a Rime Ice pack CODE against its base dictionary and imported SOURCES.
Only reject unknown syllables when all source tables are available."
  (when (and sources (cl-every #'file-readable-p sources))
    (let ((missing (delete-dups (split-string code " " t))))
      (while (and missing sources)
        (let ((file (pop sources)))
          (with-temp-buffer
            (let* ((column (cl-position "code" (rime-dict--insert-table file) :test #'equal))
                   (start (point)))
              (when column
                (setq missing (cl-remove-if
                               (lambda (syllable)
                                 (rime-dict--has-syllable-p syllable column start))
                               missing)))))))
      (when missing
        (user-error "雾凇主词库没有音节 %s。普通词请填分音节全拼（如 tuo zhu），快捷简写可填实际按键"
                    (string-join missing "、"))))))

(defun rime-dict--pinyin-readings (word files)
  "Collect explicit readings of words and characters occurring in WORD.
Scan each available pinyin table once, regardless of the length of WORD."
  (let* ((readings (make-hash-table :test #'equal))
         (chars (mapcar (lambda (ch) (encode-coding-string (char-to-string ch) 'utf-8-unix))
                        (delete-dups (string-to-list word))))
         (pattern (concat (regexp-opt chars) "+"))
         (case-fold-search nil))
    (dolist (file files)
      (when (file-readable-p file)
        (with-temp-buffer
          (let* ((columns (rime-dict--insert-table file))
                 (ci (cl-position "code" columns :test #'equal))
                 (ti (cl-position "text" columns :test #'equal)))
            (when (and ci ti)
              (rime-dict--matching-rows
               pattern columns
               (lambda (row)
                 (let ((part (nth ti row)) (code (rime-dict--code-value (nth ci row) :word)))
                   (when (and (not (string-empty-p (or code "")))
                              (string-match-p (regexp-quote part) word))
                     (cl-pushnew code (gethash part readings) :test #'equal))))))))))
    readings))

(defun rime-dict--infer-pinyin (word files)
  "Use the longest known pieces of WORD to suggest its full pinyin.
Ask about ambiguous readings and uncovered pieces.  Return nil when
no part is known, so the regular code prompt can accept the whole input."
  (let* ((readings (rime-dict--pinyin-readings word files))
         (pattern (regexp-opt (hash-table-keys readings)))
         (case-fold-search nil)
         (start 0) syllables)
    (unless (zerop (hash-table-count readings))
      (while (< start (length word))
        (let* ((match (string-match pattern word start))
               (end (cond ((null match) (length word))
                          ((= match start) (match-end 0))
                          (t match)))
               (part (substring word start end))
               (choices (reverse (gethash part readings))))
          (push (cond
                 ((null choices)
                  (rime-dict--field
                   (read-string (format "「%s」中「%s」缺少读音，请填这部分的全拼 › " word part))
                   "全拼"))
                 ((null (cdr choices)) (car choices))
                 (t (completing-read
                     (format "「%s」中「%s」的读音（第 %d 字起，%s） › "
                             word part (1+ start) (string-join choices " / "))
                     choices nil t nil nil (car choices))))
                syllables)
          (setq start end)))
      (string-join (nreverse syllables) " "))))

(defun rime-dict--read-code (word kind target hits reading-files pack-base)
  (let* ((kind-name (if (eq kind :word) "普通词" "快捷简写"))
         (codes (delete-dups
                 (cl-loop for hit in hits
                          for code = (rime-dict--hit-code hit)
                          when (and (eq (rime-dict--hit-role hit) kind)
                                    (not (string-empty-p (or code ""))))
                          collect code)))
         (inferred (and reading-files (null codes) (rime-dict--infer-pinyin word reading-files)))
         (choices (or codes (and inferred (list inferred))))
         (summary (if hits (mapconcat #'rime-dict--hit-label hits "，") "未查到整词"))
         (prompt (format "「%s」%s\n%s → %s（C-c C-w 设权重）%s › "
                         word summary kind-name (file-name-nondirectory target)
                         (cond (inferred "已补全全拼，可修改")
                               (reading-files "全拼，音节间用空格（如 tuo zhu）")
                               ((eq kind :shortcut) "快捷码（实际按键）")
                               (t "词典编码"))))
         (rime-dict--want-weight nil)
         (code (minibuffer-with-setup-hook
                   (lambda ()
                     (use-local-map (make-composed-keymap
                                     rime-dict--code-map (current-local-map))))
                 (completing-read prompt choices nil nil
                                  (and (null (cdr choices)) (car choices))
                                  'rime-dict--code-history
                                  (and reading-files (cdr choices) (car choices)))))
         (clean (rime-dict--code-value (rime-dict--field code "编码") kind)))
    (rime-dict--check-pinyin clean pack-base)
    (list clean
          (when rime-dict--want-weight
            (rime-dict--weight
             (read-string (format "「%s」 %s 的权重（可留空） › " word clean))
             kind)))))

(defun rime-dict--file-version (file)
  (when-let* ((attrs (file-attributes file)))
    (list (file-attribute-modification-time attrs) (file-attribute-size attrs)
          (file-attribute-inode-number attrs))))

(defun rime-dict--atomic-edit (file edit)
  "Run EDIT in a fresh buffer under FILE's lock.  Save iff EDIT returns t.
Keep the destination's symlink, permissions and existing line endings."
  (let* ((path (file-truename file))
         (parent (file-name-directory path))
         (visiting (find-buffer-visiting path))
         (create-lockfiles t)
         (lock (file-locked-p path))
         temp)
    (when (and visiting (buffer-modified-p visiting))
      (user-error "%s 有尚未保存的编辑" file))
    (when lock (user-error "%s 正被编辑，请稍后重试" file))
    (make-directory parent t)
    (lock-file path)
    (unwind-protect
        (let ((version (rime-dict--file-version path)))
          (with-temp-buffer
            (setq buffer-file-coding-system 'utf-8-unix)
            (when version
              (let ((coding-system-for-read 'utf-8-auto))
                (insert-file-contents path)
                (setq buffer-file-coding-system last-coding-system-used)))
            (when (funcall edit)
              (setq temp (make-temp-file (expand-file-name ".rime-dict-" parent)))
              (let ((coding-system-for-write buffer-file-coding-system))
                (write-region (point-min) (point-max) temp nil 'silent))
              (unless (equal version (rime-dict--file-version path))
                (user-error "%s 在编辑期间已被其他程序修改，请重试" file))
              (when version (set-file-modes temp (file-modes path)))
              (rename-file temp path t)
              (setq temp nil)
              (when visiting
                (with-current-buffer visiting (revert-buffer t t t)))
              t)))
      (when (and temp (file-exists-p temp)) (delete-file temp))
      (unlock-file path))))

(defun rime-dict--dict-id (file dir)
  (string-remove-suffix ".dict.yaml" (file-relative-name file dir)))

(defun rime-dict--new-table (file kind dir)
  (if (eq kind :word)
      (insert "# Rime dictionary\n# encoding: utf-8\n---\nname: "
              (json-encode-string (rime-dict--dict-id file dir))
              "\nversion: \"1.0\"\nsort: by_weight\n"
              "use_preset_vocabulary: false\ncolumns: [text, code, weight]\n...\n")
    (insert "# Rime table\n# coding: utf-8\n#@/db_name\t"
            (file-relative-name file dir) "\n#@/db_type\ttabledb\n")))

(defun rime-dict--save-entry (file kind dir word code weight)
  (rime-dict--atomic-edit
   file
   (lambda ()
     (when (zerop (buffer-size)) (rime-dict--new-table file kind dir))
     (let* ((columns (rime-dict--columns file))
            (data-start (point))
            (ti (cl-position "text" columns :test #'equal))
            (ci (cl-position "code" columns :test #'equal))
            (wi (cl-position "weight" columns :test #'equal))
            found changed)
       (unless (and ti ci) (user-error "目标 %s 需要 text 和 code 两列" file))
       (when (and weight (null wi))
         (user-error "目标 %s 没有 weight 列，请选择带权重列的目标" file))
       (rime-dict--each-row
        word columns
        (lambda (fields)
          (when (equal code (rime-dict--code-value (nth ci fields) kind))
            (setq found t)
            (when (and weight (not (equal weight (nth wi fields))))
              (while (<= (length fields) wi) (setq fields (append fields '(""))))
              (setf (nth wi fields) weight)
              (delete-region (point) (line-end-position))
              (insert (string-join fields "\t"))
              (setq changed t)))))
       (unless found
         (let ((row (make-list (1+ (max ti ci (if weight wi 0))) "")))
           (setf (nth ti row) word (nth ci row) code)
           (when weight (setf (nth wi row) weight))
           (goto-char (point-max))
           (unless (bolp) (insert "\n"))
           (when (and (string-prefix-p "#" (car row))
                      (not (save-excursion
                             (re-search-backward "^# no comment[ \t]*$" data-start t))))
             (insert "# no comment\n"))
           (insert (string-join row "\t") "\n")
           (setq changed t)))
       changed))))

(defun rime-dict--yaml-value (key)
  "Move from a position-annotated YAML mapping KEY to its value."
  (goto-char (cdr (get-text-property 0 'yaml-position key)))
  (skip-chars-forward " \t")
  (unless (eq (char-after) ?:) (user-error "无法定位 YAML 键 %s" key))
  (forward-char)
  (skip-chars-forward " \t"))

(defun rime-dict--yaml-content ()
  (while (progn (skip-chars-forward " \t\r\n")
                (eq (char-after) ?#))
    (forward-line 1)))

(defun rime-dict--mount-pack (schema pack dir &optional namespace)
  "Add PACK to SCHEMA's customization exactly once, preserving other settings."
  (let ((file (expand-file-name (concat schema ".custom.yaml") dir)))
    (rime-dict--atomic-edit
     file
     (lambda ()
       (goto-char (point-min))
       (let* ((yaml (and (> (buffer-size) 0) (yaml-parse-string-with-pos (buffer-string))))
              (patch-entry (and (listp yaml) (assoc "patch" yaml)))
              (patch (and (listp (cdr patch-entry)) (cdr patch-entry)))
              (base-key (concat (or namespace "translator") "/packs"))
              (add-key (concat base-key "/+"))
              (addition (assoc add-key patch))
              (packs (cdr addition))
              (direct (cdr (assoc base-key patch)))
              (name (json-encode-string pack))
              (rule (concat (json-encode-string add-key) ": [" name "]")))
         (unless (or (and (vectorp packs) (cl-find pack packs :test #'equal))
                     (and (vectorp direct) (cl-find pack direct :test #'equal)))
           (cond
            (addition
             (rime-dict--yaml-value (car addition))
             (rime-dict--yaml-content)
             (cond ((eq (char-after) ?\[)
                    (forward-char)
                    (insert name (if (zerop (length packs)) "" ", ")))
                   ((and (vectorp packs) (looking-at "-[ \t]"))
                    (let ((indent (current-indentation))
                          (last (aref packs (1- (length packs)))))
                      (goto-char (cdr (get-text-property 0 'yaml-position last)))
                      (end-of-line)
                      (insert "\n" (make-string indent ?\s) "- " name)))
                   (t (user-error "请将 %s 加入 %s 的 %s 列表" pack file add-key))))
            (patch-entry
             (rime-dict--yaml-value (car patch-entry))
             (cond ((save-excursion (rime-dict--yaml-content) (eq (char-after) ?{))
                    (rime-dict--yaml-content)
                    (forward-char)
                    (insert rule (if patch ", " "")))
                   ((or (eolp) (eq (char-after) ?#))
                    (let ((indent (if patch
                                      (save-excursion
                                        (goto-char (car (get-text-property
                                                         0 'yaml-position (caar patch))))
                                        (current-indentation))
                                    2)))
                      (end-of-line)
                      (insert "\n" (make-string indent ?\s) rule)))
                   (t (user-error "请将 %s 加入 %s 的 %s 列表" pack file base-key))))
            (t
             (goto-char (point-min))
             (rime-dict--yaml-content)
             (if (eq (char-after) ?{)
                 (progn (forward-char) (insert "\"patch\": {" rule "}" (if yaml ", " "")))
               (if (re-search-forward "^\\.\\.\\.[ \t]*$" nil t)
                   (beginning-of-line) (goto-char (point-max)))
               (unless (bolp) (insert "\n"))
               (insert "patch:\n  " rule "\n"))))
           (let ((result (gethash "patch" (rime-dict--yaml (buffer-string)))))
             (unless (and (hash-table-p result) (member pack (gethash add-key result)))
               (user-error "请在 %s 的 %s 中挂载 %s" file base-key pack)))
           t))))))

(defun rime-dict--deploy ()
  (if (and (boundp 'rime--lib-loaded) (not rime--lib-loaded)
           (featurep 'rime))
      (with-temp-buffer
        (activate-input-method "rime")
        (unwind-protect (rime-deploy) (deactivate-input-method)))
    (rime-deploy)))

(defun rime-dict--add (kind manual)
  (when (and rime-dict-auto-reload (not (fboundp 'rime-deploy))
             (not (and (require 'rime nil t) (fboundp 'rime-deploy))))
    (user-error "自动重载需要 emacs-rime；也可以关闭 rime-dict-auto-reload"))
  (let* ((word (rime-dict--field
                (if (use-region-p)
                    (buffer-substring-no-properties (region-beginning) (region-end))
                  (read-string "词语 › " nil 'rime-dict--word-history nil t))
                "词语"))
         (dir (rime-dict--user-dir))
         (schema (rime-dict--choose-schema dir manual))
         (catalog (rime-dict--catalog-for schema dir))
         (file (rime-dict--target schema kind catalog dir))
         (mounted (rime-dict--mounted-source file catalog))
         (pack (and (not mounted) (eq kind :word)
                    (plist-get (cdr (assoc schema rime-dict-targets)) :pack)
                    (rime-dict--catalog-namespace catalog)
                    (rime-dict--dict-id file dir)))
         (base (rime-dict--catalog-pinyin-sources catalog))
         (packs (rime-dict--catalog-pinyin-packs catalog))
         (word-table (and (eq kind :word) (car mounted)))
         (pack-base (and (or pack (member word-table packs)) base))
         (reading-files (and (or pack-base (member word-table base))
                             (delete-dups (append (list file) base packs nil)))))
    (unless (or mounted pack)
      (user-error "目标 %s 尚未挂载到方案 %s" file schema))
    (pcase-let* ((hits (rime-dict--lookup word catalog file kind))
                 (`(,code ,weight) (rime-dict--read-code word kind file hits
                                                        reading-files pack-base))
                 (changed (rime-dict--save-entry file kind dir word code weight)))
      ;; Write the dictionary before referring to it in a schema.  Retrying
      ;; after a patch or deployment failure reuses the same personal row.
      (when (and changed rime-dict-auto-reload)
        (cl-pushnew dir rime-dict--pending-reload :test #'equal))
      (when pack
        (condition-case err
            (when (rime-dict--mount-pack schema pack dir (rime-dict--catalog-namespace catalog))
              (setq changed t))
          (error (user-error "词条已保存到 %s，挂载失败：%s"
                             file (error-message-string err)))))
      (cond
       ((and rime-dict-auto-reload
             (or changed pack (member dir rime-dict--pending-reload)))
        (cl-pushnew dir rime-dict--pending-reload :test #'equal)
        (condition-case err
            (progn
              (rime-dict--deploy)
              (setq rime-dict--pending-reload (delete dir rime-dict--pending-reload))
              (message "「%s」已保存到 %s，已调用 rime-deploy" word file))
          (error (user-error "「%s」已保存到 %s，重载失败：%s" word file
                             (error-message-string err)))))
       (changed (message "「%s」已保存到 %s" word file))
       (t (message "「%s」和编码 %s 已收录在 %s" word code file))))))

;;;###autoload
(defun rime-dict-add-word (&optional choose-schema)
  "Add a regular word.  Rime Ice sources supply editable pinyin readings.
With prefix CHOOSE-SCHEMA, select the destination schema."
  (interactive "P")
  (rime-dict--add :word choose-schema))

;;;###autoload
(defun rime-dict-add-shortcut (&optional choose-schema)
  "Add a literal shortcut.  With prefix CHOOSE-SCHEMA, select its destination."
  (interactive "P")
  (rime-dict--add :shortcut choose-schema))

(provide 'rime-dict)
;;; rime-dict.el ends here
