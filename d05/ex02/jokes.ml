let jokes = [|
                "In order to understand recursion you must first understand recursion.";
                "Why do programmers always mix up Halloween and Christmas? Because Oct 31 equals Dec 25.";
                "What kind of shoes does a pedophile wear? White vans.";
                "Why programmers like UNIX:\nunzip, strip, touch, finger, grep, mount, fsck, more, yes, fsck, fsck, fsck, umount, sleep";
                "If your mom was a collection class, her insert method would be public."
            |]

let () =
    print_endline jokes.(Random.self_init ();(Random.int 5))
