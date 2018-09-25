-- Localize global functions to prevent a changing environment
-- from messing with this code. This should allow multiple
-- instances of this code to run at once. (Don't do that...)
local setfenv = setfenv
local getfenv = getfenv
local setmetatable = setmetatable
local type = type
local rawequal = rawequal
local select = select
local tostring = tostring
local newproxy = newproxy

-- Unfortunately metamethods have different ways of invoking
-- the default metamethods of values. This table provides
-- metamethods which does that, so they can be easily wrapped.
local Capsule = {}
Capsule.__metatable = 'This debug metatable is locked.'
function Capsule:__index(k) 		return self[k] 	end
function Capsule:__newindex(k, v) self[k] = v 	end
function Capsule:__call(...)		self(...) 		end
function Capsule:__concat(v)		return self .. v end
function Capsule:__unm()			return -self 	end
function Capsule:__add(v)			return self + v end
function Capsule:__sub(v)			return self - v end
function Capsule:__mul(v)			return self * v end
function Capsule:__div(v)			return self / v end
function Capsule:__mod(v)			return self % v end
function Capsule:__pow(v)			return self ^ v end
function Capsule:__tostring()		return tostring(self) end
function Capsule:__eq(v)			return self == v end
function Capsule:__lt(v)			return self < v end
function Capsule:__le(v)			return self <= v end
function Capsule:__len()			return #self 	end

-- We don't want to prevent interfaces and 'data' from being
-- garbage collected. While unwrapped 'data' shouldn't be present
-- in lua, interfaces will always be present in lua for the
-- lifetime of the data. So, we remove the data when the interfaces
-- are garbage collected, and allow interfaces to be collected.
local original = setmetatable({}, {__mode = 'k'})
local wrapper = setmetatable({}, {__mode = 'v'})

-- Since RBXLua isn't multithreaded and wrap/unwrap cannot
-- yield, i and n are fine being passed as upvalues. This
-- allows a very simple implmentation of a function wrapper
-- without the use of tables and unpack, which does not
-- always preserve nil values.
local i, n

-- This function is called to make sure no wrappers get passed
-- outside the sandbox. The only data that leaves the sandbox is
-- through metamethod and function call arguments. These arguments
-- are always unwrapped.
local function unwrap(...)
	if i > n then
		i = 1
		n = select('#', ...)
	end

	local value = select(i, ...)
	if wrapper[value] then
		value = original[wrapper[value]]
	end

	i = i + 1
	if i <= n then
		return value, unwrap(...)
	else
		return value
	end
end

-- The return value of all function and metamethod calls is wrapped.
local function wrap(...)
	if i > n then
		i = 1
		n = select('#', ...)
	end

	local value = select(i, ...)
	local wrapped = wrapper[value]
	if not wrapped then
		local vType = type(value)
		if vType == 'function' then
			wrapped = function(...)
				return wrap(value(unwrap(...)))
			end
		elseif vType == 'table' then
			wrapped = setmetatable({}, Capsule)
		elseif vType == 'userdata' then
			wrapped = setmetatable(newproxy(true), Capsule)
		else
			wrapped = value
		end

		wrapper[value] = wrapped -- Same data, same wrapper. Preserves equality tests.
		wrapper[wrapped] = wrapped -- Prevent encapsulating capsules.
		original[wrapped] = value -- store the original value to perform operations on and unwrap
	end

	i = i + 1
	if i <= n then
		return value, wrap(...)
	else
		return value
	end
end

-- Here, we ensure each metamethod is wrapped.
for key, metamethod in next, Capsule do
	Capsule[key] = wrap(metamethod)
end

-- mwhaha, now even calling rawset is sandboxed... 
-- the only functions not sandboxed are those included in the
-- locked string metatable (i.e. 'myStringVar:sub(i, j)')
return function()
	return setfenv(2, wrap(getfenv(2)))
end