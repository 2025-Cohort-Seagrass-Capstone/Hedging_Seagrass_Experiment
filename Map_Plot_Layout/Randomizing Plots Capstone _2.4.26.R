### 2025 Capstone Randomizing plots
### Created by: Savanna Saunders
### Date: 2/4/2026

#Step one: List all of the hedging types possible 
# 1. Coral Rubble
# 2. Coral Rubble
# 3. Coral Rubble
# 4. Coral & Mesh
# 5. Coral & Mesh
# 6. Coral & Mesh
# 7. Glass
# 8. Glass
# 9. Glass
# 10. Bottles
# 11. Bottles
# 12. Bottles
# 13. Control 
# 14. Control
# 15. Control


# Step 2: Create vector of 15 values
x <- 1:15

# Step 3: randomize the order of the 15 values
sample(x)

# Output from console
# > sample(x)
# [1] 14  4  1  5 10  8  9 13 15  7  2  3 11 12  6

# Step 4: Reorder the hedging types 
# 14. Control (Most North West Plot --> Top row furthest left)
# 4. Coral & Mesh
# 1. Coral Rubble
# 5. Coral & Mesh
# 10. Bottles
# 8. Glass (Middle row furthest left/West)
# 9. Glass
# 13. Control 
# 15. Control
# 7. Glass
# 2. Coral Rubble (Most South West Plot --> Bottom row furthest left)
# 3. Coral Rubble
# 11. Bottles
# 12. Bottles
# 6. Coral & Mesh
