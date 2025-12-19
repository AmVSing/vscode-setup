class TreeNode:
    def __init__(self, val=0, left=None, right=None):
        self.val = val
        self.left = left
        self.right = right

def inOrderTraversal(root):
    res =[]
    if root is None:
        return []
    res.extend(inOrderTraversal(root.left))
    res.append(root.val)
    res.extend(inOrderTraversal(root.right))
    return res





root = TreeNode(5)
root.left = TreeNode(4)
root.right = TreeNode(6)
root.right.left = TreeNode(3)
root.right.right = TreeNode(7)


print(inOrderTraversal(root))