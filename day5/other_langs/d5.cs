// day 5 part 1
var input = File.ReadAllLines(@"N:\input.txt").Select(x => Convert.ToInt16(x)).ToArray();
            var steps = 0; var pos = 0;
            while (pos < input.Length && pos >= 0)
                  steps++; pos = pos + input[pos]++;
            Console.WriteLine(steps);

// day 5 part 2
var input = File.ReadAllLines(@"N:\input.txt").Select(x => Convert.ToInt16(x)).ToArray();
        var steps = 0; var pos = 0;
        while (pos < input.Length && pos >= 0)
              steps++; pos = input[pos] >= 3 ? pos + input[pos]-- : pos + input[pos]++; 
        Console.WriteLine(steps);