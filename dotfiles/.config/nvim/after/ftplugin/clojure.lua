-- Clojure mappings
vim.keymap.set('n', '<localleader>e', 'cp', { remap = true, buffer = true })
vim.keymap.set('n', '<localleader>f', 'cpaF', { remap = true , buffer = true})
vim.keymap.set('n', '<localleader>k', ':%Eval<cr>', { buffer = true })

local threading = require('clojure.threading')
vim.keymap.set('n', '<localleader>rf', threading.thread_first, { buffer = true })
vim.keymap.set('n', '<localleader>rl', threading.thread_last, { buffer = true })
vim.keymap.set('n', '<localleader>ru', threading.unwind, { buffer = true })
vim.keymap.set('n', '<localleader>st', ':Stacktrace<cr>', { buffer = true })
vim.keymap.set('n', '<localleader>sT', ':Stacktrace!<cr>', { buffer = true })
