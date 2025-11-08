#include <ncurses.h>
#include <string>
#include <vector>

void print_menu(WINDOW* menu_win, int highlight, const std::vector<std::string>& choices) {
    box(menu_win, 0, 0);
    for (size_t i = 0; i < choices.size(); ++i) {
        if (highlight == i + 1)
            wattron(menu_win, A_REVERSE);
        mvwprintw(menu_win, i + 1, 2, choices[i].c_str());
        wattroff(menu_win, A_REVERSE);
    }
    wrefresh(menu_win);
}

int main() {
    initscr();              // Start ncurses mode
    clear();
    noecho();
    cbreak();               // Disable line buffering
    curs_set(0);            // Hide cursor
    keypad(stdscr, TRUE);   // Enable arrow keys

    int height = 10, width = 40, starty = 4, startx = 10;
    WINDOW* menu_win = newwin(height, width, starty, startx);
    keypad(menu_win, TRUE);

    std::vector<std::string> choices = {
        "Run Simulation",
        "Generate Fortran Code",
        "View Results",
        "Exit"
    };

    int choice = 0;
    int highlight = 1;

    while (true) {
        mvprintw(1, 10, "Use arrow keys to navigate. Press Enter to select.");
        refresh();
        print_menu(menu_win, highlight, choices);

        int c = wgetch(menu_win);
        switch (c) {
            case KEY_UP:
                if (highlight > 1) --highlight;
                break;
            case KEY_DOWN:
                if (highlight < choices.size()) ++highlight;
                break;
            case 10: // Enter key
                choice = highlight;
                break;
        }

        if (choice != 0) {
            clear();
            mvprintw(5, 10, "You selected: %s", choices[choice - 1].c_str());
            refresh();
            if (choices[choice - 1] == "Exit") break;
            choice = 0;
        }
    }

    delwin(menu_win);
    endwin();               // End ncurses mode
    return 0;
}
