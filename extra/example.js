{
    "locations":
    [
        {
            "locRoom": 
            {
                "roomName": "start",
                "roomDesc": "you are at the start of the demo map"
            },
            "locExits": 
            [
                {
                    "exitDir": "north",
                    "exitTo": "end"
                }
            ],
            "locObjects": ["item1"]
        }
    ],
    "objects":
    [
        {
            "objName": "item1",
            "objType": "armor",
            "objDesc": "This is item1",
            "objPickup": "You pickup item1. It feels heavier than expected",
            "objDiscard": "You drop item1 and it makes a thunk as it lands"
        }
    ],
    "players":
    [
        {
            "playerAt": "start",
            "playerHas": ["item1"]
        }
    ],
    "monsters":
    [
        {
            "monsterDesc": "a grumpy gnome",
            "monsterAt": "start",
            "monsterHas": ["item1"]
        }
    ]
}
