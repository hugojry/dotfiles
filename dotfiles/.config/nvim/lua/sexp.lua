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

---@return integer
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

---@return integer, integer
local function get_cur()
  local row, col = unpack(a.nvim_win_get_cursor(0))
  row = row - 1
  return row, col
end

---@param row integer 0-indexed row
---@param col integer 0-indexed col
local function set_cur(row, col)
  a.nvim_win_set_cursor(0, { row + 1, col })
end

function M.adjacent_sexp(is_forward)
  local row, col = get_cur()

  local node = ts.get_node()
  if not node then return end

  -- Find a node that represents a type of sexp
  node = find_node_types(sexp_node_types, node)
  if not node then return end

  -- If the cursor is between two siblings of the current node then move
  -- to the start of the adjacent sibling
  local nodes = is_between(node, row, col)
  if nodes then
    local dest_row, dest_col = nodes[is_forward and 2 or 1]:range()
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

  set_cur(dest_row, dest_col)
end

M.forward_sexp = function()
  for _ = 1, vim.v.count1 do M.adjacent_sexp(true) end
end

M.backward_sexp = function()
  for _ = 1, vim.v.count1 do M.adjacent_sexp(false) end
end

---@param start_row integer
---@param start_col integer
---@param end_row integer
---@param end_col integer
---@param dest_row integer
---@param dest_col integer
local function move_range(
  start_row, start_col, end_row, end_col, dest_row, dest_col
)
  local text = a.nvim_buf_get_text(0, start_row, start_col, end_row, end_col, {})
  -- dest starts after range starts
  if cur_order(start_row, start_col, dest_row, dest_col) > 0 then
    a.nvim_buf_set_text(0, dest_row, dest_col, dest_row, dest_col, text)
    if cur_order(end_row, end_col, dest_row, dest_col) > 0 then
      a.nvim_buf_set_text(0, start_row, start_col, end_row, end_col, {})
    else
      a.nvim_buf_set_text(0, start_row, start_col, dest_row, dest_col, {})
    end
  else
    a.nvim_buf_set_text(0, start_row, start_col, end_row, end_col, {})
    a.nvim_buf_set_text(0, dest_row, dest_col, dest_row, dest_col, text)
  end
end

---@param node TSNode
---@param count integer
local function slurp_left(node, count)
  local sib = node:prev_named_sibling()
  if not sib then return end
  for _ = 1, count - 1 do
    local sib_next = sib:prev_named_sibling()
    if not sib_next then
      break
    else
      sib = sib_next
    end
  end

  local row, col = node:range()
  local start_row, start_col = sib:range()
  move_range(row, col, row, col + 1, start_row, start_col)
  set_cur(start_row, start_col)
end

---@param node TSNode
---@param count integer
local function slurp_right(node, count)
  local sib = node:next_named_sibling()
  if not sib then return end
  for _ = 1, count - 1 do
    local sib_next = sib:next_named_sibling()
    if not sib_next then
      break
    else
      sib = sib_next
    end
  end

  local _, _, row, col = node:range()
  local _, _, end_row, end_col = sib:range()
  move_range(row, col - 1, row, col, end_row, end_col)
  set_cur(end_row, end_col - (row == end_row and 1 or 0))
end

---@param node TSNode
---@param count integer
local function barf_left(node, count)
  local named_child_count = node:named_child_count()
  if named_child_count < 2 then return end

  local retained_child = node:named_child(
    math.max(0, named_child_count - 1 - count)
  )
  if not retained_child then return end

  local _, _, row, col = node:range()
  local _, _, end_row, end_col = retained_child:range()
  move_range(row, col - 1, row, col, end_row, end_col)
  set_cur(end_row, end_col)
end

---@param node TSNode
---@param count integer
local function barf_right(node, count)
  local named_child_count = node:named_child_count()
  if named_child_count < 2 then return end

  local retained_child = node:named_child(
    math.min(count, named_child_count - 1)
  )
  if not retained_child then return end

  local row, col = node:range()
  local start_row, start_col = retained_child:range()
  move_range(row, col, row, col + 1, start_row, start_col)
  set_cur(start_row, start_col - (row == start_row and 1 or 0))
end

---@param is_left boolean
---@param count integer
local function slurp_barf(is_left, count)
  local node = ts.get_node()
  if not node then return end

  node = find_node_types(list_node_types, node)
  if not node then return end

  local row, col = get_cur()

  local cur_text = a.nvim_buf_get_text(0, row, col, row, col + 1, {})[1]
  if openers[cur_text] then
    (is_left and slurp_left or barf_right)(node, count)
  elseif closers[cur_text] then
    (is_left and barf_left or slurp_right)(node, count)
  else
    (is_left and slurp_left or slurp_right)(node, count)
  end
end

function M.slurp_barf_left_repeat()
  slurp_barf(true, vim.v.count1)
end

function M.slurp_barf_right_repeat()
  slurp_barf(false, vim.v.count1)
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
