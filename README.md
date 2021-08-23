# Flat CFMM

CFMM between two tokens (FA1.2 / FA2 each) using the isoutility curve U(x,y) = (x+y)^8 - (x-y)^8
This curve has the benefit of being extremely flat around x = y, at which point dx U / dy U = -1.
This makes it suitable to create a CFMM between two assets which ought to be pegged to one another.

Based on the generic cfmm in the ctez repo which is itself based on dexter v2.

Barely tested!

## The curve

![Curve showing the constant product curve, the constant sum curve, and the compromise](https://user-images.githubusercontent.com/1591742/130374091-5c447f97-8bb1-407c-b97d-eb463fdd8666.png)

To give some sense, when the two assets are held in equal amounts, 78% of the pool can be bought for a slippage of less than 5%.

![Slippage for constant product, sum, and the compromise](https://user-images.githubusercontent.com/1591742/130374323-7e040a68-197a-4f3b-85f4-a5d625d8b220.png)


