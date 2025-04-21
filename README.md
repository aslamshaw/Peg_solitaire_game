# Recursive Peg Solitaire Solver (Fortran 90)

This repository contains a **Fortran 90** implementation of a **recursive backtracking algorithm** to solve the classic game of **Peg Solitaire**.

## 🧩 Game Rules (Simplified)

Peg Solitaire is a single-player puzzle game where the goal is to eliminate pegs from a board by jumping one peg over another into an empty space. The game continues until only one peg remains, ideally in the center.

### Board Layout:
- The board is 7x7 with some positions marked as **empty** (non-playable spaces).
- Initially, the board has 32 pegs placed, with the center space empty.
  
### Objective:
- Remove pegs by jumping over adjacent pegs into empty spaces. 
- You must aim to leave only one peg on the board by performing a series of jumps.

---

## 🚀 How It Works

The program uses a **recursive backtracking algorithm** to explore all potential moves in Peg Solitaire. The recursive function tries all possible moves, checks if they lead to a solution, and backtracks if necessary.

---

## 🖥️ How to Run

1. **Download** or **clone** the repository.
2. Make sure you have a **Fortran 90** compiler installed (e.g., **gfortran**).
3. **Compile** the program.
4. **Run** the program.

   The program will output the sequence of moves and visualize the board at each step. It also records the computational and visualization times in `data_time_rec.txt` and the board's movements in `data1.txt`.

---

## 🛠️ File Structure

- `peg_solitare.f90`: Main program for initializing the board and calling the recursive solver.
- `recursive_function.f90`: The recursive function that solves the puzzle by exploring all valid moves.
- `data1.txt`: Logs the state of the board at each step for visualization.
- `data_time_rec.txt`: Logs the computation and visualization times.

---

## ⚙️ Output Format

- **Console Output**:
  - The program prints the final board configuration and the list of moves that led to the solution.
  - It will indicate if no solution is found.
  
- **Files**:
  - `data1.txt` contains the board states at each step.
  - `data_time_rec.txt` logs the time taken to compute and visualize the solution.

---
