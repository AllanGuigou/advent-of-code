
string[] input = System.IO.File.ReadAllLines("sample1.txt");

// var root = new Node(-1, null, null);
// root = root with { left = new Node(-1, new Node(0, null, null), new Node(3, null, null)) };
// root = root with { right = new Node(-1, new Node(4, null, null), new Node(-1, new Node(7, null, null), new Node(9, null, null))) };
// Print(root);
// Console.WriteLine();

var node = input.Select(x => createNode(x.ToCharArray())).Aggregate((acc, x) => addition(acc, x));

Print(node);
Console.WriteLine();

void Print(Node? node)
{
	if (node is null) return;
	if (node.value < 0)
	{
		Console.Write('[');
	}
	Print(node.left);
	if (node.value >= 0)
	{
		Console.Write(node.value);
	}
	else
	{
		Console.Write(',');
	}
	Print(node.right);
	if (node.value < 0)
	{
		Console.Write(']');
	}
}

Node createNode(char[] input)
{
	var isPair = input[0] == '[';
	if (!isPair && input.Length == 1)
	{
		return new Node(System.Int32.Parse(input), null, null);
	}

	var stack = new Stack<char>();
	var pairCommaIndex = 0;

	for (var i = 0; i < input.Length; i++)
	{
		switch (input[i], stack.Count())
		{
			case ('[', _):
				stack.Push(input[i]);
				break;
			case (']', _):
				stack.Pop();
				break;
			case (',', 1):
				pairCommaIndex = i;
				break;
		}
	}

	if (pairCommaIndex == 0) throw new Exception();

	return new Node(-1,
	createNode(input.Skip(1).Take(pairCommaIndex - 1).ToArray()),
	createNode(input.Skip(pairCommaIndex + 1).Take(input.Length - pairCommaIndex - 2).ToArray())
    );
}

Node addition(Node left, Node right)
{
	return new Node(-1, left, right);
}
public record Node(int value, Node? left, Node? right);


