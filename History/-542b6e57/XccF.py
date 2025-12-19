class ListNode:
    def __init__(self, val=0, next=None):
        self.val = val
        self.next = next

xs = [1,2,3,4,5,6]

dummy = ListNode()
new_head = dummy
for x in xs:
    dummy.val = x
    dummy.next = ListNode()
    dummy = dummy.next

while new_head is not None:
    print(new_head)
    new_head = new_head.next
