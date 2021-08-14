db.createCollection("price_volume", {
    validator: {
        $jsonSchema: {
            bsonType: "object",
            required: ["token", "hourstamp", "seconds", "prices", "volumes"],
            properties: {
                token: {
                    bsonType: "string",
                    minLength: 1,
                    description: "token symbol",
                },
                hourstamp: {
                    bsonType: "int",
                    minimum: 0,
                    description: "timestamp // 3600 of all datapoints",
                },
                seconds: {
                    bsonType: "array",
                    maxItems: 3600,
                    items: {
                        bsonType: "int",
                        minimum: 0,
                        maximum: 3599,
                    },
                    description: "timestamp % 3600 of the consecutive datapoints",
                },
                prices: {
                    bsonType: "array",
                    maxItems: 3600,
                    items: {
                        bsonType: "double",
                        minimum: 0.0,
                    },
                    description: "token prices at the consecutive datapoints",
                },
                volumes: {
                    bsonType: "array",
                    maxItems: 3600,
                    items: {
                        bsonType: "double",
                        minimum: 0.0,
                    },
                    description: "token transaction volumes at the consecutive datapoints",
                },
            }
        }
    }
})
