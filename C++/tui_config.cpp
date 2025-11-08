#include <ncurses.h>
#include <fstream>
#include <string>
#include <vector>
#include <sstream>

void load_config(const std::string& filename, std::vector<std::string>& keys, std::vector<std::string>& values) {
    std::ifstream infile(filename);
    std::string line, key, value;
    while (std::getline(infile, line)) {
        std::istringstream iss(line);
        if (iss >> key >> value) {
            keys.push_back(key);
            values.push_back(value);
        }
    }
}

void save_config(const std::string& filename, const std::vector<std::string>& keys, const std::vector<std::string>& values) {
    std::ofstream outfile(filename);
    for (size_t i = 0; i < keys.size(); ++i) {
        outfile << keys[i] << " " << values[i] << "\n";
    }
}

int main() {
    std::vector<std::string> keys, values;
    load_config("config.txt", keys, values);

    initscr();
    noecho();
    cbreak();
    keypad(stdscr, TRUE);
    curs_set(0);

    int highlight = 0;
    int ch;

    while (true) {
        clear();
        mvprintw(0, 2, "Edit Config File (Enter to edit, F2 to save, ESC to exit)");

        for (size_t i = 0; i < keys.size(); ++i) {
            if (highlight == i)
                attron(A_REVERSE);
            mvprintw(i + 2, 4, "%s = %s", keys[i].c_str(), values[i].c_str());
            if (highlight == i)
                attroff(A_REVERSE);
        }

        ch = getch();
        if (ch == KEY_UP && highlight > 0) highlight--;
        else if (ch == KEY_DOWN && highlight < keys.size() - 1) highlight++;
        else if (ch == 10) { // Enter key
            echo();
            curs_set(1);
            char buffer[100];
            mvprintw(keys.size() + 4, 2, "New value for %s: ", keys[highlight].c_str());
            move(keys.size() + 4, 22);
            clrtoeol(); // Clear line before input
            getnstr(buffer, 99);
            values[highlight] = std::string(buffer);
            noecho();
            curs_set(0);
        }
        else if (ch == KEY_F(2)) {
            save_config("config.txt", keys, values);
            mvprintw(keys.size() + 6, 2, "Config saved! Press any key...");
            getch();
        }
        else if (ch == 27) { // ESC key
            break;
        }
    }

    endwin();
    return 0;
}
