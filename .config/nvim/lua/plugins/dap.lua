local dap = require('dap')

local dbg_leader = '<leader>d'

vim.keymap.set('n', dbg_leader .. 'r', dap.restart)
vim.keymap.set('n', dbg_leader .. 'R', dap.terminate)
vim.keymap.set('n', dbg_leader .. 'o', dap.repl.toggle)

vim.keymap.set('n', dbg_leader .. 'c', dap.continue)
vim.keymap.set('n', dbg_leader .. 'C', dap.run_to_cursor)
vim.keymap.set('n', dbg_leader .. 's', dap.step_over)
vim.keymap.set('n', dbg_leader .. 'S', dap.step_back)
vim.keymap.set('n', dbg_leader .. 'i', dap.step_into)
vim.keymap.set('n', dbg_leader .. 'I', dap.step_out)

vim.keymap.set('n', dbg_leader .. 'bb', dap.toggle_breakpoint)
vim.keymap.set('n', dbg_leader .. 'bs', dap.set_breakpoint)
vim.keymap.set('n', dbg_leader .. 'bD', dap.clear_breakpoints)
vim.keymap.set('n', dbg_leader .. 'bp', function()
	dap.set_breakpoint(nil, nil, vim.fn.input('Log point message: '))
end)

local telescope_dap = require('telescope').extensions.dap
vim.keymap.set('n', dbg_leader .. 'd', telescope_dap.commands)
vim.keymap.set('n', dbg_leader .. 'v', telescope_dap.variables)
vim.keymap.set('n', dbg_leader .. 'f', telescope_dap.variables)
vim.keymap.set('n', dbg_leader .. 'bl', telescope_dap.list_breakpoints)


local dapui = require('dapui')
dapui.setup({
	-- Set icons to characters that are more likely to work in every terminal.
	--    Feel free to remove or use ones that you like more! :)
	--    Don't feel like these are good choices.
	icons = { expanded = 'v', collapsed = '>', current_frame = '*' },
	controls = {
		icons = {
			pause = '||',
			play = '>',
			step_into = '→',
			step_over = '↓',
			step_out = '←',
			step_back = '↑',
			run_last = '>>',
			terminate = '⏹',
			disconnect = ":(", -- )
		},
	},
})

vim.keymap.set('n', dbg_leader .. 'u', dapui.toggle)
dap.listeners.after.event_initialized['dapui_config'] = dapui.open
dap.listeners.before.event_terminated['dapui_config'] = dapui.close
dap.listeners.before.event_exited['dapui_config'] = dapui.close
