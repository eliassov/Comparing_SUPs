# R Package Development Practice Notes & Setup History

This document serves as a permanent record of our conversation, setup steps, and tutorial guide. This file will be pushed to GitHub to preserve your work and learning history across ephemeral virtual machine sessions.

---

## 1. Project Concept & Structure

To practice building R packages and working with Statistical Disclosure Control (sdc), we are converting a subset of the functions in this repository into a clean, buildable R package.

### Target Files for Practice:
1. **`R/create_microdata.R`**: Generates mock microdata for testing.
2. **`R/hiers.R`**: Utilities to work with hierarchies.
3. **`R/add_response.R`**: Generates Pareto Type II (Lomax) response values.

### Package Dependencies:
- **`SSBtools`**: Auto-hierarchy, RowGroups, SortRows, and MakeMicro utilities.
- **`sdcHierarchies`**: Hierarchy structure manipulation and conversion.
- **`VGAM`**: Generating Lomax (Pareto Type II) distributions for cell responses.

---

## 2. Git & Fork Configuration

Since your Onyxia virtual environment is ephemeral, we redirected your workspace to point to your own personal GitHub fork so your practice is safely stored under your account.

- **Colleague's branch checked out**: `dev2_oyl`
- **Your Personal Fork URL**: `https://github.com/eliassov/Comparing_SUPs.git`
- **Active Git command run to update remote**:
  ```bash
  git remote set-url origin https://github.com/eliassov/Comparing_SUPs.git
  ```

---

## 3. Configuration Files Added

We added two crucial configuration files in the root directory:

### `.Rproj` File (`Comparing_SUPs.Rproj`)
Ensures RStudio recognizes this directory as an R project and activates package building tools.
Key setting:
```ini
BuildType: Package
```

### `DESCRIPTION` File
Specifies package metadata, version, and dependencies.
```dcf
Package: ComparingSUPs
Title: Comparing Secondary Cell Suppression Methods
Version: 0.1.0
Authors@R: c(
    person("Michel", "Moehler", email = "mamoeh@users.noreply.github.com", role = "aut"),
    person("Øyvind", "Langsrud", email = "oyl@ssb.no", role = "aut"),
    person("Elias", "S", email = "rgo@ssb.no", role = "cre")
    )
Description: An R package to practice generating microdata and hierarchies 
    and comparing different secondary cell suppression methods (e.g., GAUSS, MODULAR).
License: MIT
Encoding: UTF-8
LazyData: true
Imports:
    SSBtools,
    sdcHierarchies,
    VGAM
RoxygenNote: 7.2.3
```

---

## 4. Key RStudio Workflows & Keyboard Shortcuts

Once you reopen your project in RStudio, use these key shortcuts/commands:

| Action | RStudio Click | Shortcut | R Command |
| :--- | :--- | :--- | :--- |
| **Load Package Code** | `Build` -> `Load All` | `Ctrl + Shift + L` | `devtools::load_all()` |
| **Document Functions** | `Build` -> `More` -> `Document` | `Ctrl + Shift + D` | `devtools::document()` |
| **Check Package Standards** | `Build` -> `Check` | `Ctrl + Shift + E` | `devtools::check()` |
| **Install Package** | `Build` -> `Install and Restart` | `Ctrl + Shift + B` | `devtools::install()` |

---

## 5. Next Steps

1. **Reopen RStudio in the Project:** Click **Project: (None)** in the top right, select **Open Project...**, and open `Comparing_SUPs.Rproj`.
2. **Commit these changes** so they are saved to your fork:
   ```bash
   git add Comparing_SUPs.Rproj DESCRIPTION practice_notes.md
   git commit -m "Initialize R project, DESCRIPTION, and practice notes"
   git push origin dev2_oyl
   ```
