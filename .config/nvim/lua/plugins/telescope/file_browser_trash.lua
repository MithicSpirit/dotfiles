--[[ Thanks @stelcodes on github for this function. Should be removed if/when
https://github.com/nvim-telescope/telescope-file-browser.nvim/pull/196 is merged
]]

local a = vim.api

local fb_utils = require "telescope._extensions.file_browser.utils"

local actions = require "telescope.actions"
local state = require "telescope.state"
local action_state = require "telescope.actions.state"
local action_utils = require "telescope.actions.utils"
local action_set = require "telescope.actions.set"
local config = require "telescope.config"
local transform_mod = require("telescope.actions.mt").transform_mod

local Path = require "plenary.path"
local popup = require "plenary.popup"

return function(prompt_bufnr)
  local current_picker = action_state.get_current_picker(prompt_bufnr)
  local finder = current_picker.finder
  local quiet = current_picker.finder.quiet
  local trash_cmd = nil
  if vim.fn.executable "trash" == 1 then
    trash_cmd = "trash"
  elseif vim.fn.executable "gio" == 1 then
    trash_cmd = "gio"
  end
  if not trash_cmd then
    fb_utils.notify("actions.trash", { msg = "Cannot locate a valid trash executable!", level = "WARN", quiet = quiet })
    return
  end
  local selections = fb_utils.get_selected_files(prompt_bufnr, true)
  if vim.tbl_isempty(selections) then
    fb_utils.notify("actions.trash", { msg = "No selection to be trashed!", level = "WARN", quiet = quiet })
    return
  end

  local files = vim.tbl_map(function(sel)
    return sel.filename:sub(#sel:parent().filename + 2)
  end, selections)

  for _, sel in ipairs(selections) do
    if sel:is_dir() then
      local abs = sel:absolute()
      local msg
      if finder.files and Path:new(finder.path):parent():absolute() == abs then
        msg = "Parent folder cannot be trashed!"
      end
      if not finder.files and Path:new(finder.cwd):absolute() == abs then
        msg = "Current folder cannot be trashed!"
      end
      if msg then
        fb_utils.notify("actions.trash", { msg = msg .. " Prematurely aborting.", level = "WARN", quiet = quiet })
        return
      end
    end
  end

  local trashed = {}

  local message = "Selections to be trashed: " .. table.concat(files, ", ")
  fb_utils.notify("actions.trash", { msg = message, level = "INFO", quiet = quiet })
  -- TODO fix default vim.ui.input and nvim-notify 'selections to be deleted' message
  vim.ui.input({ prompt = "Trash selections [y/N]: " }, function(input)
    vim.cmd [[ redraw ]] -- redraw to clear out vim.ui.prompt to avoid hit-enter prompt
    if input and input:lower() == "y" then
      for _, p in ipairs(selections) do
        local is_dir = p:is_dir()
        local cmd = nil
        if trash_cmd == "gio" then
          cmd = { "gio", "trash", "--", p:absolute() }
        else
          cmd = { trash_cmd, "--", p:absolute() }
        end
        vim.fn.system(cmd)
        if vim.v.shell_error == 0 then
          table.insert(trashed, p.filename:sub(#p:parent().filename + 2))
          -- clean up opened buffers
          if not is_dir then
            fb_utils.delete_buf(p:absolute())
          else
            fb_utils.delete_dir_buf(p:absolute())
          end
        else
          local msg = "Command failed: " .. table.concat(cmd, " ")
          fb_utils.notify("actions.trash", { msg = msg, level = "WARN", quiet = quiet })
        end
      end
      local msg = nil
      if next(trashed) then
        msg = "Trashed: " .. table.concat(trashed, ", ")
      else
        msg = "No selections were successfully trashed"
      end
      fb_utils.notify("actions.trash", { msg = msg, level = "INFO", quiet = quiet })
      current_picker:refresh(current_picker.finder)
    else
      fb_utils.notify("actions.trash", { msg = "Trashing selections aborted!", level = "INFO", quiet = quiet })
    end
  end)
end
