(local _this ...)

(fn no-sep [name opts]
  (vim.tbl_deep_extend :force
    {1 name :padding {:left 1 :right 0} :separator ""}
    (or opts {})))

{1 :nvim-lualine/lualine.nvim
 :event :UIEnter
 :opts
   #{:options
       {:icons_enabled false
        :theme (require (.. _this :.catppuccin))
        :component_separators {:left "" :right ""}
        :section_separators {:left "" :right ""}
        :disabled_filetypes {:statusline {} :winbar {}}
        :ignore_focus {}
        :always_divide_middle true
        :globalstatus false
        :refresh {:statusline 500 :tabline 1000 :winbar 1000}}
       :sections
       {:lualine_a [(require (.. _this :.mode))]
        :lualine_b [{1 :filename
                     :file_status true
                     :newfile_status true
                     :path 4
                     :sorting_target 80
                     :symbols {:modified "+" :readonly "x"
                               :unnamed "[No Name]" :newfile "[New]"}}]
        :lualine_c [(no-sep :location)
                    :progress]
        :lualine_x [:filesize
                    (no-sep :encoding)
                    :fileformat
                    :filetype]
        :lualine_y [:diff :diagnostics]
        :lualine_z []}
       :inactive_sections
       {:lualine_a []
        :lualine_b []
        :lualine_c [{1 :filename
                     :file_status true
                     :newfile_status true
                     :path 4
                     :sorting_target 80
                     :symbols {:modified "+" :readonly "@"
                               :unnamed "[No Name]" :newfile "[New]"}}
                    :location]
        :lualine_x [:filetype]
        :lualine_y []
        :lualine_z []}}}
