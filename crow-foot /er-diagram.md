
## 1. The Main People & Things
### 👩‍🏫 Lecturers

These are the teachers.
Each teacher has:

* An **ID** (their roll number)
* A **Name**
* A **Department** (like Maths, CS, etc.)

### 📚 Lectures

These are the classes.
Each class has:

* An **ID**
* A **Title** (like “Math 101”)
* A **Room**
* A **Time**

### 🧑‍🎓 Students

These are the kids in college.
Each student has:

* An **ID**
* A **Name**
* A **Subject** (what they study)

### 📝 Tutorials

These are small practice classes for a lecture.
Each tutorial has:

* The **Lecture’s ID** (so we know which class it belongs to)
* A **Room**
* A **Time**

### 🧑‍🏫 Tutors

These are special students who help teach.
Each tutor:

* Is already a **Student**
* Has a **Salary** (they get paid 💸)

---

## 2. Who Does What With Whom?

Now the fun part: **relationships** 🤝

### A. Lecturers ↔ Lectures

Teachers can teach **many** classes.
Classes can have **many** teachers.

So we make a helper table called **Lecturing**:

* Lecturer-Id
* Lecture-Id

This just says:
👉 “This teacher teaches this class.”

---

### B. Students ↔ Lectures

Students can attend **many** classes.
Each class must have **at least one** student.

So we use **Attending**:

* Student-Id
* Lecture-Id

This says:
👉 “This student goes to this class.”

---

### C. Lectures ↔ Tutorials

One big class can have **many** small tutorials.
Each tutorial belongs to **one** lecture only.

So:

* 1 Lecture → Many Tutorials
* But each Tutorial → Only 1 Lecture

Easy.

---

### D. Tutors ↔ Tutorials

Tutors can teach **many** tutorials.
Each tutorial can have **many** tutors.

So we use **Tutoring**:

* Student-Id
* Lecture-Id
* Room
* Time

This says:
👉 “This tutor teaches this tutorial in this room at this time.”

---

### E. Students ↔ Tutors

Not every student is a tutor.
But every tutor **must** be a student.

So:

* Student → Maybe Tutor
* Tutor → Always Student

Like:

> Every superhero is a human, but not every human is a superhero 🦸‍♂️

---

## 3. Keys (The Important Rules 🔑)

* **Primary Key (PK)** = Unique ID (like a roll number)
* **Foreign Key (FK)** = Points to another table

Example:

* Tutorial has **Lecture-Id**
  That tells us *which lecture it belongs to.*

---

## 4. Crow’s Foot Notation (The Little Claws 🐾)

This just shows:

* One
* Many
* Optional
* Mandatory

Like:

* 👤 → 🐾🐾 = One to Many
* 👤 → 👤 = One to One

It’s just drawing rules, not scary.

---

## Final Baby Summary 

* **Teachers** teach **Classes**
* **Students** attend **Classes**
* **Classes** have **Tutorials**
* **Some students** become **Tutors**
* **Tutors** teach **Tutorials**

