local dap = require('dap')

local function dbg_nmap(key, command)
	vim.keymap.set('n',  '<leader>d' .. key, command)
end

dbg_nmap('r', dap.restart)
dbg_nmap('R', dap.terminate)
dbg_nmap('o', dap.repl.toggle)

dbg_nmap('c', dap.continue)
dbg_nmap('C', dap.run_to_cursor)
dbg_nmap('s', dap.step_over)
dbg_nmap('S', dap.step_back)
dbg_nmap('i', dap.step_into)
dbg_nmap('I', dap.step_out)

dbg_nmap('bb', dap.toggle_breakpoint)
dbg_nmap('bs', dap.set_breakpoint)
dbg_nmap('bD', dap.clear_breakpoints)
dbg_nmap('bp', function()
	dap.set_breakpoint(nil, nil, vim.fn.input('Log point message: '))
end)

local telescope_dap = require('telescope').extensions.dap
dbg_nmap('d', telescope_dap.commands)
dbg_nmap('v', telescope_dap.variables)
dbg_nmap('f', telescope_dap.variables)
dbg_nmap('bl', telescope_dap.list_breakpoints)


local dapui = require('dapui')
dapui.setup({
	icons = { expanded = 'v', collapsed = '>', current_frame = '*' },
	controls = {
		icons = {
			pause = '||',
			play = '|>',
			step_into = '->',
			step_over = '\\/',
			step_out = '<-',
			step_back = '/\\',
			run_last = '>>',
			terminate = '[]',
			disconnect = '<>',
		},
	},
})

dbg_nmap('u', dapui.toggle)
dap.listeners.after.event_initialized['dapui_config'] = dapui.open
dap.listeners.before.event_terminated['dapui_config'] = dapui.close
dap.listeners.before.event_exited['dapui_config'] = dapui.close
