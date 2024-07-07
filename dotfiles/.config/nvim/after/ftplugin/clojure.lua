-- Clojure mappings
vim.keymap.set('n', '<localleader>e', 'cp', { remap = true, buffer = true })
vim.keymap.set('n', '<localleader>f', 'cpaF', { remap = true , buffer = true})
vim.keymap.set('n', '<localleader>k', ':%Eval<cr>', { buffer = true })

local util = require('util')

-- fireplace is using these mappings, so take them back
vim.api.nvim_create_autocmd('User', {
  pattern = 'FireplaceActivate',
  callback = function()
    vim.keymap.set('n', '[d', util.goto_prev_diagnostic, { buffer = true })
    vim.keymap.set('n', ']d', util.goto_next_diagnostic, { buffer = true })
  end
})

local threading = require('clojure.threading')
vim.keymap.set('n', '<localleader>rf', threading.thread_first, { buffer = true })
vim.keymap.set('n', '<localleader>rl', threading.thread_last, { buffer = true })
vim.keymap.set('n', '<localleader>ru', threading.unwind, { buffer = true })
