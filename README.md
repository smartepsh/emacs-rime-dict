# rime-dict

在 Emacs 里选中一个词，录入普通词典或快捷简写表。没有选区时，直接在 minibuffer 输入词语。

| 命令 | 用途 | 编码示例 |
| --- | --- | --- |
| `M-x rime-dict-add-word` | 加入普通词库，参与组句 | 给「拓竹」确认读音 `tuo zhu`，日常打字时敲自然码双拼 `tovu` |
| `M-x rime-dict-add-shortcut` | 设置实际按键对应的固定短语 | 敲 `xmzb` 得到「项目周报」 |

输入编码前，提示中会显示已收录该词的源文件、编码和显式权重。同类词表只有一个已知编码时会预填，有多个时可以用 `TAB` 补全，也可以输入新编码。

用雾凇加普通词时，选中词语即可自动补全全拼。工具先查整词读音。遇到新词，会优先匹配本地词库中已注音的词组，再用单字读音补齐。例如，新词里有「重庆」，就能沿用 `chong qing`。若剩下的字或词有多个读音，minibuffer 会列出读音供你选择。遇到缺少注音的片段，只需补填该片段。补好的整串全拼可以编辑，按 `RET` 保存。

雾凇的个人词库沿用主词库 `rime_ice` 的音节。主词库及其子表的源文件齐全时，工具会在写入个人扩展词库前检查音节，并提示哪一段拼写需要修改。

- `RET` 保存。新增时权重可留空，目标文件已有同词同码时保留原权重。
- `C-c C-w` 接受当前编码，接着在 minibuffer 输入权重。上游已收录的词可以写进个人目标提权。目标里已有同词同码时更新原行。
- `C-u` 加上任一命令可以临时选择目标方案。正常加词时使用当前可确定的方案，无法确定时使用 `rime-dict-default-schema`。检测到未映射的方案时，选择框会预选默认方案。

## 安装

需要 Emacs 27.1 或更高版本及 [GNU ELPA 的 yaml 包](https://elpa.gnu.org/packages/yaml.html) 1.2.4 或更高版本。用 `M-x package-install-file` 选择本目录的 `rime-dict.el` 即可安装。使用 `straight.el` 时，也可以直接加载本地源码，将下面的路径设为本地项目目录。

```elisp
(use-package yaml :straight t)

(use-package rime-dict
  :straight nil
  :load-path "~/Kenton/rime-dict"
  :commands (rime-dict-add-word rime-dict-add-shortcut))
```

## 重载

`rime-dict-auto-reload` 在包首次加载时按环境取默认值。

| 设置 | 保存后的行为 |
| --- | --- |
| `t`（已安装 `emacs-rime` 时默认） | 文件有变化后自动调用 `rime-deploy`。相同词码、相同权重直接结束。 |
| `nil`（未安装 `emacs-rime` 时默认） | 只写入文件，可稍后在使用的 Rime 前端部署。 |

显式设为 `t` 却无法加载 `emacs-rime` 时，命令会在写入前报错。若词条已保存而部署失败，提示会告知文件位置，重试同一词条会重新部署。

独立使用时，指定前端的 Rime 用户目录。共享词库放在其他目录时，还可以设置 `rime-dict-shared-data-dir`。

```elisp
(setq rime-dict-auto-reload nil
      rime-dict-user-data-dir "~/Library/Rime")
```

## 方案与目标文件

默认配置适用于雾凇的自然码双拼，路径相对于 `rime-user-data-dir` 或显式指定的 `rime-dict-user-data-dir`。

```elisp
(setq rime-dict-default-schema "double_pinyin"
      rime-dict-targets
      '(("double_pinyin"
         :word "personal_pinyin.dict.yaml" :pack t
         :shortcut auto)))
```

`:word` 指定个人 `.dict.yaml`。`:pack t` 会在首次保存时创建缺少的个人词库，并把它挂到方案翻译器的 `packs` 列表。自然码双拼的快捷简写使用 `:shortcut auto`，从实际部署配置的 `custom_phrase/user_dict` 得到 `custom_phrase_double.txt`。也可以填写已经挂载的 `.txt` 文件名。个人短语表使用 `stabledb`，由工具负责写入。

使用形码时，可在同一张映射表中登记它已挂载的个人码表，只填写需要的 `:word` 或 `:shortcut`。形码按该码表的规则录入。`:pack t` 适用于主词库已经包含相关音节或码元的方案。自定义前端如能提供实时方案 ID，可将 `rime-dict-current-schema-function` 设为返回该 ID 的无参函数。

查词按词面精确匹配当前方案可读的源词表，包括主词库、`import_tables` 子表、`packs` 和文本短语表。程序按各文件自己的 `columns` 读取。缺少编码或权重时，提示会标明。写入使用目标文件自己的列顺序，显式设置权重需要目标包含 `weight` 列。用户学习记录和仅提供编译产物的词库不在文本查找范围内。

## 测试

安装 `yaml` 后，可在项目目录运行测试。

```sh
emacs -Q --batch --eval '(package-initialize)' -L . \
  -l test/rime-dict-test.el -f ert-run-tests-batch-and-exit
```

用 `straight.el` 安装 `yaml` 时，将 `--eval '(package-initialize)'` 换为 `-L ~/.emacs.d/straight/build/yaml`。
