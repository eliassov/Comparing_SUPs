# Explanation: "Handling of bogus codes is not implemented" Error in sdcTable

## 1. What is a "Bogus" Code?
In the context of the `sdcTable` R package, a **bogus code** (or redundant code) is a code/category in a hierarchy that does not add any new level of classification or partition. 

The most common occurrence is when a parent category has **exactly one child category** in the hierarchy. For example, in the `minimal` hierarchy (`var1`):
```text
TOTAL
 └── 01
      └── 0101
 └── 02
      └── 0201
```
Here:
- `TOTAL` splits into two children: `01` and `02`. This is a valid split.
- `01` has exactly one child: `0101`.
- `02` has exactly one child: `0201`.

Because `01` and `0101` represent the exact same level of aggregation, and `02` and `0201` represent the exact same level of aggregation, they are redundant.

---

## 2. Why Does the Error Occur?
When computing safety intervals via linear programming (LP) using the `sdcTable` package, the function `sdcTable::attack()` is called under the hood (specifically in `R/intervals_by_sdcTable.R` inside the `sdcTable_attack` function).

The function checks whether the hierarchy contains redundant nodes by comparing the table structures returned with and without duplicate nodes:
```R
gdf_TRUE <- sdcTable:::g_df(object, addDups = TRUE, addNumVars = FALSE)
gdf      <- sdcTable:::g_df(object, addDups = FALSE, addNumVars = FALSE)
if (nrow(gdf_TRUE) != nrow(gdf)) {
  stop("Handling of bogus codes is not implemented")
}
```
If there are bogus/redundant codes, `nrow(gdf_TRUE)` will not equal `nrow(gdf)`. Since `sdcTable_attack` does not support processing these redundant rows directly, it halts with the error:
`Error in sdcTable_attack(...): Handling of bogus codes is not implemented`

---

## 3. How to Resolve the Error
The project already includes a dedicated utility function designed to solve this exact issue: `remove_bogus()` (defined in `R/remove_bogus.R`). 

This function uses `SSBtools::AutoHierarchies` to find all redundant categories and filter them out of the hierarchy representation before they are initialized in the SDC pipeline.

### Solution in `analysis/run_native_comparison.R`:
To fix the error and allow the safety interval calculations to complete successfully, you can apply `remove_bogus` to your loaded hierarchies before initializing the pipeline:

```R
# 2. Load standard packaged hierarchies
hiers <- list(
  var1 = read_hier("minimal", path = file.path(project_dir, "inst/extdata")),
  var2 = read_hier("table_header", path = file.path(project_dir, "inst/extdata"))
)

# --- ADD THIS LINE TO REMOVE REDUNDANT/BOGUS CODES ---
hiers <- lapply(hiers, remove_bogus)
```

Applying `remove_bogus` simplifies the hierarchy to:
```text
TOTAL
 └── 01
 └── 02
```
This removes the single-child redundancy, ensuring that `sdcTable_attack` runs without any errors and correctly calculates all safety intervals.
