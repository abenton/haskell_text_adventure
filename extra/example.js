{
    "locations":
    [
        {
            "name": "start",
            "description": "you are at the start of the demo map",
            "exits": 
            {
                "north": "end"
                "south": "other"
            },
            "objects": ["item1", "item2"]
        },
        {
            "name": "end",
            "description": "you are at the end of the demo map",
            "exits": 
            {
            },
            "objects": []
        },
        {
            "name": "other",
            "description": "you are in another room in the map",
            "exits": 
            {
                "north": "start"
            },
            "objects": []
        }
    ],
    "objects":
    [
        {
            "name": "item1",
            "type": "armor",
            "description": "This is item1",
            "pickup": "You pickup item1. It feels heavier than expected",
            "discard": "You drop item1 and it makes a thunk as it lands"
        },
        {
            "name": "item2",
            "type": "key",
            "description": "This is item2",
            "pickup": "You pickup item2. It's light and airy",
            "discard": "You release item2 and it floats away"
        },
        {
            "name": "item1",
            "type": "weapon",
            "description": "This is item3",
            "pickup": "You grab item3 and relize too late how hot it was",
            "discard": "You drop item3 and soothe your burning hand"
        },
    ],
    "players":
    [
        {
            "inventory": ["item3"],
            "location": "start"
        }
    ],
    "monsters":
    [
        {
            "description": "a grumpy gnome",
            "inventory": [],
            "location": "other"
        }
    ]
}
