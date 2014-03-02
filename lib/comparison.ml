
class comparison = 
	object(this)

		method int_equal (x:int) (y:int) = 
			x == y

		method int_not_equal (x:int) (y:int) = 
			x != y

		method int_less_than (x:int) (y:int) = 
			x < y

		method int_greater_than (x:int) (y:int) = 
			x > y

		method int_less_than_or_equal (x:int) (y:int) = 
			x <= y

		method int_greater_than_or_equal (x:int) (y:int) = 
			x >= y

	end;;