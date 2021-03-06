defmodule Sorting do

  defmodule BubbleSort do

    def sort(unsorted), do: do_sort(unsorted, [])

    defp do_sort([], sorted), do: sorted

    defp do_sort(unsorted, sorted) do
      {unsorted, new_sorted} = do_sort(unsorted)
      do_sort(unsorted, new_sorted ++ sorted)
    end

    defp do_sort([only]) do
      {[], [only]}
    end

    defp do_sort([first, second]) when first > second do
      {[second, first], []}
    end

    defp do_sort([first, second]) when first <= second do
      {[], [first, second]}
    end

    defp do_sort([first, second | rest]) when first > second do
      {unsorted, sorted} = do_sort([first | rest])
      {[second | unsorted], sorted}
    end

    defp do_sort([first, second | rest]) when first <= second do
      case do_sort([second | rest]) do
        {[], sorted} -> {[], [first | sorted]}
        {unsorted, sorted} -> {[first | unsorted], sorted}
      end
    end
  end

  defmodule QuickSort do

    def sort(unsorted), do: do_sort(unsorted)

    defp do_sort([]), do: []

    defp do_sort(unsorted) do
      [pivot | unsorted] = :lists.reverse(unsorted)
      {lower, higher, same} = do_sort(:lists.reverse(unsorted), pivot)
      do_sort(lower) ++ [pivot | same] ++ do_sort(higher)
    end

    defp do_sort([], _), do: {[], [], []}

    defp do_sort([first | rest], pivot) when first > pivot do
      {lower, higher, same} = do_sort(rest, pivot)
      {lower, [first | higher], same}
    end

    defp do_sort([first | rest], pivot) when first < pivot do
      {lower, higher, same} = do_sort(rest, pivot)
      {[first | lower], higher, same}
    end

    defp do_sort([first | rest], pivot) when first == pivot do
      {lower, higher, same} = do_sort(rest, pivot)
      {lower, higher, [first | same]}
    end
  end

  # defmodule HeapSort do

  # end

  defmodule Config,  do: defstruct timed: false, count: 30, loops: 30

  def test(module, config \\ %Config{}) do
    f = if config.timed do
      fn unsorted -> measure(fn -> module.sort(unsorted) end) end
    else
      fn unsorted -> module.sort(unsorted) end
    end
    for _ <- 0..config.loops, do: validate(f.(random_list(config)))
  end

  defp random_list(config), do: for _ <- 0..config.count, do: :rand.uniform(150)

  defp validate([]) do
    true
  end

  defp validate([_first]) do
    true
  end

  defp validate([first, second | rest]) when first <= second do
    validate([second | rest])
  end
  
  defp measure(f) do
    {time, value} = f |> :timer.tc()
    IO.puts "taken = #{time}"
    value
  end
end
