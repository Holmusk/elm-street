module Tests.Golden exposing (goldenOneTypeJson)

goldenOneTypeJson : String
goldenOneTypeJson =
    """
    {
        "tag": "OneType",
        "prims": {
            "maybe": 12,
            "list": [
                1,
                2,
                3,
                4,
                5
            ],
            "tag": "Prims",
            "time": "2019-02-22T00:00:00Z",
            "text": "heh",
            "result": {
                "Left": 666
            },
            "pair": [
                "o",
                false
            ],
            "triple": [
                "o",
                false,
                [0]
            ],
            "float": 36.6,
            "char": "a",
            "int": 42,
            "bool": true,
            "unit": []
        },
        "myUnit": {
            "tag": "MyUnit",
            "contents": []
        },
        "myResult": {
            "tag": "Err",
            "contents": "clashing test"
        },
        "userRequest": {
            "tag": "UserRequest",
            "example": {
                "Right": {
                    "tag": "Blocked"
                }
            },
            "ids": [
                "1",
                "2"
            ],
            "limit": 123
        },
        "age": 18,
        "newtype":  666,
        "newtypeList": [123],
        "oneConstructor":  "OneConstructor",
        "user": {
            "status": "Approved",
            "tag": "User",
            "age": 100,
            "name": "not-me",
            "id": "1"
        },
        "id": "myId",
        "requestStatus": "Reviewing",
        "guests": [
            {
                "tag": "Regular",
                "contents": [
                    "nice",
                    7
                ]
            },
            {
                "tag": "Visitor",
                "contents": "new-guest"
            },
            {
                "tag": "Blocked"
            }
        ]
    }
    """
