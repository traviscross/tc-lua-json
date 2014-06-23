-- Copyright 2014 Travis Cross <tc@traviscross.com>
--
-- Permission is hereby granted, free of charge, to any person obtaining a copy
-- of this software and associated documentation files (the "Software"), to deal
-- in the Software without restriction, including without limitation the rights
-- to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
-- copies of the Software, and to permit persons to whom the Software is
-- furnished to do so, subject to the following conditions:
--
-- The above copyright notice and this permission notice shall be included in
-- all copies or substantial portions of the Software.
--
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
-- IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
-- FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
-- AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
-- LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
-- OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
-- THE SOFTWARE.

-- parser

function char(s,p)
  return string.sub(s,p,p)
end

function expect_char(s,p,c)
  if char(s,p) == c then
    return c,true,nil,s,p+1
  end
  return nil,true,"expected "..c.." at "..p,s,p
end

function pat_find(s,p,pat)
  local n=string.find(s,pat,p)
  if n then
    return n
  else
    return #s+1
  end
end

function parse_object(s,p)
  if char(s,p) ~= "{" then
    return nil,false,"not an object",s,p
  end
  p=p+1
  local r={}
  if char(s,p) == "}" then
    return r,true,nil,s,p+1
  end
  while true do
    local k,v,a,e,_
    k,a,e,s,p=parse_string(s,p)
    if e then return r,a,e,s,p end
    _,a,e,s,p = expect_char(s,p,":")
    if e then return r,a,e,s,p end
    v,a,e,s,p=parse_value(s,p)
    if e then return r,a,e,s,p end
    r[k]=v
    if char(s,p) == "}" then
      return r,true,nil,s,p+1
    elseif char(s,p) == "," then
      p=p+1
    else
      return nil,true,"expected a comma or object end at "..p,s,p
    end
  end
end

function parse_array(s,p)
  if char(s,p) ~= "[" then
    return nil,false,"not an array",s,p
  end
  p=p+1
  local r={}
  if char(s,p) == "]" then
    return r,true,nil,s,p+1
  end
  while true do
    local v,a,e
    v,a,e,s,p=parse_value(s,p)
    if e then return v,a,e,s,p end
    table.insert(r,v)
    if char(s,p) == "]" then
      return r,true,nil,s,p+1
    elseif char(s,p) == "," then
      p=p+1
    else
      return nil,true,"expected a comma or array end at "..p,s,p
    end
  end
end

function parse_value(s,p)
  local v,a,e
  v,a,e,s,p=parse_string(s,p)
  if a then return v,a,e,s,p end
  v,a,e,s,p=parse_number(s,p)
  if a then return v,a,e,s,p end
  v,a,e,s,p=parse_object(s,p)
  if a then return v,a,e,s,p end
  v,a,e,s,p=parse_array(s,p)
  if a then return v,a,e,s,p end
  v,a,e,s,p=parse_symbol(s,p)
  if a then return v,a,e,s,p end
  return nil,false,"not a value",s,p
end

function parse_symbol(s,p)
  if not string.find(char(s,p),"[a-zA-Z]") then
    return nil,false,"not a symbol",s,p
  end
  if string.sub(s,p,p+3) == "true" then
    return true,true,nil,s,p+4
  elseif string.sub(s,p,p+4) == "false" then
    return false,true,nil,s,p+5
  elseif string.sub(s,p,p+3) == "null" then
    return nil,true,nil,s,p+4
  end
  return nil,true,"invalid symbol at "..p,s,p
end

function parse_string(s,p)
  if string.sub(s,p,p) ~= "\"" then
    return nil,false,"not a string",s,p
  end
  p=p+1
  local i=p l=#s
  while true do
    if char(s,p) == "\\" then
      p=p+1
    elseif char(s,p) == "\"" then
      break
    elseif p >= l then
      return nil,true,"unterminated string starting at "..i-1,s,p
    end
    p=p+1
  end
  return string.sub(s,i,p-1),true,nil,s,p+1
end

function parse_number(s,p)
  if not string.find(char(s,p),"[-0-9]",1)  then
    return nil,false,"not a number",s,p
  end
  local e=pat_find(s,p,"[^-+0-9.eE]")
  local v=tonumber(string.sub(s,p,e-1))
  if v then
    return v,true,nil,s,e
  else
    return nil,true,"invalid number value at "..p,s,p
  end
end

function parse_json(s)
  local v,a,e,s,p=parse_value(s,1)
  if e then return nil,e end
  return v,nil
end

-- printer

function json_from(x)
  local k=type(x)
  if k == "string" then
    return "\""..x.."\""
  elseif k == "table" then
    if x[1] then
      return json_from_list(x)
    else
      return json_from_hash(x)
    end
  elseif k == "number" then
    return tostring(x)
  else
    return tostring(x)
  end
end

function json_from_list(l)
  if not l then return "" end
  local m
  for _,v in pairs(l) do
    if m then m=m.."," else m="" end
    m=m..json_from(v)
  end
  return "["..(m or "").."]"
end

function json_from_hash(tab)
  if not tab then return "" end
  local m
  for k,v in pairs(tab) do
    if m then m=m.."," else m="" end
    m=m.."\""..tostring(k).."\":"..json_from(v)
  end
  return "{"..(m or "").."}"
end