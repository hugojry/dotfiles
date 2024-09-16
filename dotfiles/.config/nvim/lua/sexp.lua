local M = {}
local a = vim.api
local ts = vim.treesitter

local sexp_node_types = {
  list_lit = true,
  vec_lit  = true,
  map_lit  = true,
  kwd_lit  = true,
  sym_lit  = true,
  num_lit  = true,
  str_lit  = true
}

local list_node_types = {
  list_lit = true,
  vec_lit  = true,
  map_lit  = true
}

local openers = {
  ["("] = true,
  ["["] = true,
  ["{"] = true
}

local closers = {
  [")"] = true,
  ["]"] = true,
  ["}"] = true
}


---@param node TSNode
---@return TSNode?
local function find_node_types(types, node)
  while not types[node:type()] do
    local parent = node:parent()
    if parent then
      node = parent
    else
      return
    end
  end
  return node
end

local function cur_order(row_a, col_a, row_b, col_b)
  return (row_a == row_b and col_b - col_a) or row_b - row_a
end

---@param node TSNode
---@return TSNode[]?
local function is_between(node, row, col)
  local child_iter = node:iter_children()
  local prev

  for child in child_iter do
    local start_row, start_col, end_row, end_col = child:range()

    -- Then the cursor is between children of the current node
    if cur_order(row, col, start_row, start_col) > 0 then
      return { prev, child }
    -- Then the cursor falls inside one of the children of the current node
    elseif cur_order(row, col, end_row, end_col) > 0 then
      return
    end

    prev = child
  end
end

function M.adjacent_sexp(is_forward)
  local row, col = unpack(a.nvim_win_get_cursor(0))
  row = row - 1

  local node = ts.get_node()
  if not node then return end

  -- Find a node that represents a type of sexp
  node = find_node_types(sexp_node_types, node)
  if not node then return end

  -- If the cursor is between two siblings of the current node then move
  -- to the start of the adjacent sibling
  local nodes = is_between(node, row, col)
  if nodes then
    local before, after = unpack(nodes)
    local dest_row, dest_col = (is_forward and after or before):range()
    a.nvim_win_set_cursor(0, { dest_row + 1, dest_col })
    return
  end

  if not is_forward then
    local node_start_row, node_start_col = node:range()
    if row ~= node_start_row or col ~= node_start_col then
      a.nvim_win_set_cursor(0, { node_start_row + 1, node_start_col })
      return
    end
  end

  local dest_row, dest_col
  local f = is_forward and 'next_named_sibling' or 'prev_named_sibling'
  local sib = node[f](node)
  if not sib then
    if is_forward then
      _, _, dest_row, dest_col = node:range()
    else
      dest_row, dest_col = node:range()
    end
    if is_forward and a.nvim_get_mode().mode ~= 'no' then
      dest_col = dest_col - 1
    end
  else
    dest_row, dest_col = sib:range()
  end

  a.nvim_win_set_cursor(0, { dest_row + 1, dest_col })
end

local function vim_count()
  return vim.v.count == 0 and 1 or vim.v.count
end

M.forward_sexp = function()
  for _ = 1, vim_count() do M.adjacent_sexp(true) end
end

M.backward_sexp = function()
  for _ = 1, vim_count() do M.adjacent_sexp(false) end
end

---@param node TSNode
---@return string
local function left_paren(node) return ts.get_node_text(node, 0):sub(1, 1) end
---@param node TSNode
---@return string
local function right_paren(node) return ts.get_node_text(node, 0):sub(-1, -1) end

---@param is_left boolean
---@param node TSNode the node the paren is currently attached to
local function slurp(is_left, node)
  local f = is_left and 'prev_named_sibling' or 'next_named_sibling'
  local sib = node[f](node)
  if not sib then return end

  local paren
  if is_left then paren = left_paren(node) else paren = right_paren(node) end

  if is_left then
    local row, col = node:range()
    local start_row, start_col = sib:range()
    a.nvim_buf_set_text(0, row, col, row, col + 1, {})
    a.nvim_buf_set_text(0, start_row, start_col, start_row, start_col, { paren })
    a.nvim_win_set_cursor(0, { start_row + 1, start_col })
  else
    local _, _, row, col = node:range()
    local _, _, end_row, end_col = sib:range()
    a.nvim_buf_set_text(0, end_row, end_col, end_row, end_col, { paren })
    a.nvim_buf_set_text(0, row, col - 1, row, col, {})
    a.nvim_win_set_cursor(0, { end_row + 1, end_col - 1 })
  end
end

-- This puts the paren 1 char too far to the left in scratch when right barfing
---@param is_left boolean
---@param node TSNode the node the paren is currently attached to
local function barf(is_left, node)
  local named_child_count = node:named_child_count()
  if named_child_count < 2 then return end

  local paren
  if is_left then paren = right_paren(node) else paren = left_paren(node) end

  if is_left then
    local second_last_child = node:named_child(named_child_count - 2)
    if not second_last_child then return end

    local _, _, row, col = node:range()
    local _, _, end_row, end_col = second_last_child:range()
    a.nvim_buf_set_text(0, end_row, end_col, end_row, end_col, { paren })
    a.nvim_buf_set_text(0, row, col - 1, row, col, {})
    a.nvim_win_set_cursor(0, { end_row + 1, end_col })
  else
    local second_child = node:named_child(1)
    if not second_child then return end

    local row, col = node:range()
    local start_row, start_col = second_child:range()
    a.nvim_buf_set_text(0, row, col, row, col + 1, {})
    a.nvim_buf_set_text(0, start_row, start_col - 1, start_row, start_col - 1, { paren })
    a.nvim_win_set_cursor(0, { start_row + 1, start_col - 1})
  end
end

---@param is_left boolean
local function slurp_barf(is_left)
  local node = ts.get_node()
  if not node then return end

  node = find_node_types(list_node_types, node)
  if not node then return end

  local row, col = unpack(a.nvim_win_get_cursor(0))
  row = row - 1

  local cur_text = a.nvim_buf_get_text(0, row, col, row, col + 1, {})[1]
  if openers[cur_text] then
    (is_left and slurp or barf)(is_left, node)
  elseif closers[cur_text] then
    (is_left and barf or slurp)(is_left, node)
  end
end

-- when count is not 1 then does not work
function M.slurp_barf_left_repeat()
  for _ = 1, vim_count() do slurp_barf(true) end
end

-- when count is not 1 then does not work
function M.slurp_barf_right_repeat()
  for _ = 1, vim_count() do slurp_barf(false) end
end

function M.slurp_barf_left()
  vim.go.operatorfunc = "v:lua.require'sexp'.slurp_barf_left_repeat"
  return "g@l"
end

function M.slurp_barf_right()
  vim.go.operatorfunc = "v:lua.require'sexp'.slurp_barf_right_repeat"
  return "g@l"
end

return M
