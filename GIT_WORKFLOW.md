# 🧠 Git + RStudio Collaboration Workflow

This checklist helps us stay in sync when working together on this R project using Git and GitHub.

---

## ✅ DAILY WORKFLOW

### 🔁 Before you start working
- [ ] Pull the latest changes from GitHub  
  *(RStudio → Git tab → Pull)*

---

### ✍️ While working
- [ ] Make changes in your R scripts / notebooks
- [ ] Save files regularly (`Ctrl+S` or `Cmd+S`)

---

### 💾 After finishing edits
- [ ] Stage changed files  
  *(Git tab → check the boxes)*  
- [ ] Write a clear commit message  
  *(e.g., "Add seasonal plot", "Fix NA filtering")*
- [ ] Commit the changes  
  *(Click "Commit")*

---

### 📤 Final step
- [ ] Push to GitHub  
  *(Git tab → Push)*

✅ You're done — your teammates can now Pull your changes!

---

## 📝 Good practices

- Write **clear commit messages**
- Commit often — smaller commits are better than huge ones
- Don't push `.RData`, `.Rhistory`, or `output/` folders
- Use `.gitignore` to avoid clutter

---

## 🆘 If something breaks

1. **Run:**  
   ```r
   usethis::git_sitrep()