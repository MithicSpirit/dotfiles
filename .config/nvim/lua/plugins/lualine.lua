local get_mode = require('plugins.lualine.mode')
local lualine_buffers = require('plugins.lualine.buffers')

require('lualine').setup({
	options = {
		icons_enabled = false,
		theme = require('plugins.lualine.catppuccin'),
		component_separators = {left = '', right = ''},
		section_separators = {left = '', right = ''},
		disabled_filetypes = {statusline = {}, winbar = {}},
		ignore_focus = {},
		always_divide_middle = true,
		globalstatus = false,
		refresh = {
			statusline = 500,
			tabline = 1000,
			winbar = 1000,
		}
	},
	sections = {
		lualine_a = {get_mode},
		lualine_b = {},
		lualine_c = {
			{'location', separator = {left = ''}, padding = {
				left = 1, right = 0
			}},
			'progress',
			'searchcount',
		},
		lualine_x = {
			'filesize',
			{'encoding', separator = {right = ''}, padding = {
				left = 1, right = 0
			}},
			'fileformat',
			'filetype',
		},
		lualine_y = {'diff', 'diagnostics'},
		lualine_z = {},
	},
	inactive_sections = {
		lualine_a = {},
		lualine_b = {},
		lualine_c = {'location'},
		lualine_x = {'filetype'},
		lualine_y = {},
		lualine_z = {},
	},
	tabline = {},
	winbar = {
		lualine_a = {
			{
				'buffers',
				mode = 2,
				show_filename_only = false,
				hide_filename_extension = false,
				use_mode_colors = true,
				max_length = function()
					return vim.o.columns  * 0.8
				end,
				show_modified_status = true,
				symbols = {
					modified = ' +',
					alternate_file = '#',
					directory = '',
				}
			},
		},
		lualine_b = {},
		lualine_c = {},
		lualine_x = {},
		lualine_y = {'branch'},
		lualine_z = {},
	},
	inactive_winbar = {
		lualine_a = {},
		lualine_b = {},
		lualine_c = {
			{
				'filename',
				path = 4,
				file_status = true,
				shorting_target = 15,
				symbols = {
					modified = '+',
					readonly = '',
					unnamed = '[No Name]',
					newfile = '[New]',
				}
			},
		},
		lualine_x = {},
		lualine_y = {},
		lualine_z = {},
	},
	extensions = {},
})


-- override normal binding to use lualine indices instead
vim.keymap.set('n', '<Tab>', function()
	local cur_bufpos = lualine_buffers.bufnr2pos[vim.fn.bufnr()]
	local next_bufnr = lualine_buffers.bufpos2nr[cur_bufpos + 1]
		or lualine_buffers.bufpos2nr[1]
	if next_bufnr == nil then
		vim.cmd.bnext()
		return
	end
	vim.api.nvim_set_current_buf(next_bufnr)
end)
vim.keymap.set('n', '<S-Tab>', function()
	local cur_bufpos = lualine_buffers.bufnr2pos[vim.fn.bufnr()]
	local prev_bufnr = lualine_buffers.bufpos2nr[cur_bufpos - 1]
		or lualine_buffers.bufpos2nr[#lualine_buffers.bufpos2nr]
	if prev_bufnr == nil then
		vim.cmd.bprevious()
		return
	end
	vim.api.nvim_set_current_buf(prev_bufnr)
end)
vim.keymap.set('n', '<leader><Tab>', function()
	if vim.v.count > 0 then
		lualine_buffers.buffer_jump(vim.v.count, '!')
	elseif not pcall(function() vim.cmd.edit('#') end) then
		vim.api.nvim_err_writeln('E23: No alternate file')
	end
end)
