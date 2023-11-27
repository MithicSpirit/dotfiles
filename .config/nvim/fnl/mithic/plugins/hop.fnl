(fn hop [typ dir opts]
  #(let [cole vim.o.conceallevel]
     (set vim.opt.conceallevel 0)
     ((. (require :hop) typ)
      (vim.tbl_deep_extend :force
        {:direction (. (require :hop.hint) :HintDirection dir)}
        (or opts {})))
     (set vim.opt.conceallevel cole)))

[{1 "smoka7/hop.nvim"
  :config true
  :keys
    [{1 "gsf" 2 (hop :hint_char1 :AFTER_CURSOR) :mode ""}
     {1 "gsF" 2 (hop :hint_char1 :BEFORE_CURSOR) :mode ""}
     {1 "gss" 2 (hop :hint_char2 :AFTER_CURSOR) :mode ""}
     {1 "gsS" 2 (hop :hint_char2 :BEFORE_CURSOR) :mode ""}
     {1 "gst" 2 (hop :hint_char1 :AFTER_CURSOR {:hint_offset -1}) :mode ""}
     {1 "gsT" 2 (hop :hint_char1 :BEFORE_CURSOR {:hint_offset 1}) :mode ""}
     {1 "gsj" 2 (hop :hint_vertical :AFTER_CURSOR) :mode ""}
     {1 "gsk" 2 (hop :hint_vertical :BEFORE_CURSOR) :mode ""}
     {1 "gsw" 2 (hop :hint_words :AFTER_CURSOR) :mode ""}
     {1 "gsb" 2 (hop :hint_words :BEFORE_CURSOR) :mode ""}
     {1 "gs/" 2 (hop :hint_patterns :AFTER_CURSOR) :mode ""}
     {1 "gs?" 2 (hop :hint_patterns :BEFORE_CURSOR) :mode ""}]}]
