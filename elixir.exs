
ExUnit.start()

defmodule NinetyNine do

  # P01 get last element in the list

	def last_element [] do nil end
  def last_element [x | []]  do x end
  def last_element [_ | xs ] do last_element xs end


  # P02 get second to last element of a list 

  def second_to_last [] do nil end
  def second_to_last [_x | []]  do nil end
  def second_to_last [x | [_]] do x end
  def second_to_last [_x | xs] do second_to_last xs end

  # P03 get nth element from the list

  def nth_element(0, _) do nil end
  def nth_element(_, []) do nil end
  def nth_element(1, [x | _]) do x end
  def nth_element(n, [_ | xs]) do nth_element((n-1), xs) end

  # P04 get length of a list

  defp aux_length(acc, []) do acc end
  defp aux_length(acc, [_x | xs]) do aux_length((acc + 1), xs) end

  def list_length(lst) do
    aux_length(0, lst)
  end
end


defmodule Tests do
  use ExUnit.Case

  test "P01" do
    assert NinetyNine.last_element([1, 2, 3, 4]) == 4
    assert NinetyNine.last_element([1]) == 1
    assert NinetyNine.last_element([]) == nil
  end

  test "P02" do
    assert NinetyNine.second_to_last([1, 2, 3, 4]) == 3
    assert NinetyNine.second_to_last([]) == nil
    assert NinetyNine.second_to_last([1]) == nil
  end

  test "P03" do
    assert NinetyNine.nth_element(2, [1, 2, 3, 4]) == 2
    assert NinetyNine.nth_element(3, []) == nil
    assert NinetyNine.nth_element(0, [1, 2, 3]) == nil
  end

  test "P04" do
    assert NinetyNine.list_length([1, 2, 3]) == 3
    assert NinetyNine.list_length([]) == 0
  end
end

defmodule Main do
  def run  do
    IO.puts "Test results"
  end
end

Main.run()
