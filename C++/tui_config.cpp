// config_tui.cpp
// Compile: g++ -std=c++17 -O2 -lncurses -o config_tui config_tui.cpp

#include <ncurses.h>
#include <string>
#include <vector>
#include <map>
#include <fstream>
#include <sstream>
#include <algorithm>
#include <ctime>
#include <cstdio>

struct Line {
    enum Type { COMMENT, BLANK, KV } type;
    std::string raw;       // original line (for comments/blanks)
    std::string key;       // for KV
    std::string value;     // for KV
};

class Config {
public:
    std::vector<Line> lines;
    std::map<std::string, size_t> keyIndex; // map key -> index in lines

    bool load(const std::string &path) {
        std::ifstream f(path);
        if (!f.is_open()) return false;
        lines.clear();
        std::string ln;
        size_t idx = 0;
        while (std::getline(f, ln)) {
            Line L;
            if (isBlank(ln)) {
                L.type = Line::BLANK;
                L.raw = ln;
            } else if (isComment(ln)) {
                L.type = Line::COMMENT;
                L.raw = ln;
            } else {
                auto p = splitKV(ln);
                L.type = Line::KV;
                L.key = p.first;
                L.value = p.second;
                // store index for quick lookup (last occurrence wins)
                keyIndex[L.key] = idx;
            }
            lines.push_back(L);
            ++idx;
        }
        return true;
    }

    bool save(const std::string &path) {
        std::ofstream f(path);
        if (!f.is_open()) return false;
        for (auto &L : lines) {
            if (L.type == Line::KV) {
                f << L.key << " = " << L.value << "\n";
            } else {
                f << L.raw << "\n";
            }
        }
        f.close();
        return true;
    }

    std::vector<std::pair<std::string,std::string>> kvList() const {
        std::vector<std::pair<std::string,std::string>> out;
        for (auto &L : lines) {
            if (L.type == Line::KV) out.emplace_back(L.key, L.value);
        }
        return out;
    }

    bool set(const std::string &key, const std::string &value) {
        auto it = keyIndex.find(key);
        if (it != keyIndex.end()) {
            lines[it->second].value = value;
            return true;
        } else {
            // add at end
            Line L;
            L.type = Line::KV;
            L.key = key;
            L.value = value;
            keyIndex[key] = lines.size();
            lines.push_back(L);
            return true;
        }
    }

    bool remove(const std::string &key) {
        auto it = keyIndex.find(key);
        if (it == keyIndex.end()) return false;
        size_t idx = it->second;
        lines.erase(lines.begin() + idx);
        // rebuild keyIndex (cheap but simple)
        keyIndex.clear();
        for (size_t i=0;i<lines.size();++i) {
            if (lines[i].type == Line::KV) keyIndex[lines[i].key] = i;
        }
        return true;
    }

private:
    static bool isBlank(const std::string &s) {
        for (char c : s) if (!isspace((unsigned char)c)) return false;
        return true;
    }
    static bool isComment(const std::string &s) {
        for (char c : s) {
            if (isspace((unsigned char)c)) continue;
            return c=='#' || c==';';
        }
        return false;
    }
    static std::pair<std::string,std::string> splitKV(const std::string &s) {
        std::string t = s;
        size_t eq = t.find('=');
        if (eq == std::string::npos) {
            // treat entire line as key with empty value
            std::string k = trim(t);
            return {k, ""};
        }
        std::string left = trim(t.substr(0, eq));
        std::string right = trim(t.substr(eq+1));
        return {left, right};
    }
    static std::string trim(std::string s) {
        auto l = s.find_first_not_of(" \t\r\n");
        auto r = s.find_last_not_of(" \t\r\n");
        if (l==std::string::npos) return "";
        return s.substr(l, r-l+1);
    }
};

// ------------------ TUI ------------------

class Tui {
public:
    Tui(Config &c, const std::string &path): conf(c), filepath(path) {
        kv = conf.kvList();
        selected = 0;
        top = 0;
    }
    void run() {
        initscr();
        cbreak();
        noecho();
        keypad(stdscr, TRUE);
        curs_set(0);
        int h, w;
        getmaxyx(stdscr, h, w);
        statusWinH = 3;

        drawAll();
        bool running = true;
        while (running) {
            int ch = getch();
            switch (ch) {
                case 'q':
                    running = confirmQuit();
                    break;
                case KEY_UP:
                case 'k':
                    moveUp();
                    break;
                case KEY_DOWN:
                case 'j':
                    moveDown();
                    break;
                case 10: // Enter
                case KEY_ENTER:
                    editValue();
                    break;
                case 'a':
                    addKey();
                    break;
                case 'd':
                    deleteKey();
                    break;
                case 's':
                    saveFile();
                    break;
                case KEY_RESIZE:
                    clear();
                    break;
                default:
                    break;
            }
            drawAll();
        }

        endwin();
    }

private:
    Config &conf;
    std::string filepath;
    std::vector<std::pair<std::string,std::string>> kv;
    size_t selected;
    size_t top; // first visible
    int statusWinH;

    void drawAll() {
        clear();
        int h,w;
        getmaxyx(stdscr, h, w);
        int listH = h - statusWinH - 1;
        mvprintw(0,0,"Config editor - %s", filepath.c_str());
        // draw list box
        int n = kv.size();
        if (selected >= n && n>0) selected = n-1;
        if (selected < top) top = selected;
        while (top + listH <= selected && top+1 <= selected) ++top;

        for (int i=0;i<listH && (size_t)i + top < kv.size(); ++i) {
            size_t idx = top + i;
            auto &p = kv[idx];
            std::string line = p.first + " = " + p.second;
            if (line.size() > (size_t)w-2) line = line.substr(0, w-5) + "...";
            if (idx == selected) {
                attron(A_REVERSE);
                mvprintw(1+i, 0, "%s", line.c_str());
                attroff(A_REVERSE);
            } else {
                mvprintw(1+i, 0, "%s", line.c_str());
            }
        }
        // status / help
        int sy = h - statusWinH + 1;
        mvhline(h-statusWinH, 0, '-', w);
        mvprintw(sy, 0, "Enter: edit  a: add  d: delete  s: save  q: quit");
        if (!kv.empty() && selected < kv.size()) {
            mvprintw(sy+1, 0, "Selected: %s", kv[selected].first.c_str());
        } else {
            mvprintw(sy+1, 0, "No keys");
        }
        refresh();
    }

    void moveUp() {
        if (kv.empty()) return;
        if (selected > 0) --selected;
    }
    void moveDown() {
        if (kv.empty()) return;
        if (selected + 1 < kv.size()) ++selected;
    }

    void editValue() {
        if (kv.empty()) return;
        echo();
        curs_set(1);
        int h,w; getmaxyx(stdscr,h,w);
        // small prompt at bottom
        std::string prompt = "Edit value for '" + kv[selected].first + "': ";
        int y = h - statusWinH - 1;
        move(y, 0);
        clrtoeol();
        mvprintw(y, 0, "%s", prompt.c_str());
        char buf[4096];
        // prefill by printing current value then letting user type -- simpler: use wgetnstr
        int maxlen = std::min((int)sizeof(buf)-1, w - (int)prompt.size() - 2 + 4096);
        // Print current
        printw("%s", kv[selected].second.c_str());
        move(y, (int)prompt.size() + (int)kv[selected].second.size());
        refresh();
        // get input: we will read a line into buf
        wgetnstr(stdscr, buf, 4095);
        std::string newval(buf);
        // update both kv & config
        kv[selected].second = newval;
        conf.set(kv[selected].first, newval);
        noecho();
        curs_set(0);
    }

    void addKey() {
        echo();
        curs_set(1);
        int h,w; getmaxyx(stdscr,h,w);
        int y = h - statusWinH - 1;
        move(y, 0);
        clrtoeol();
        mvprintw(y, 0, "New key name: ");
        char kb[512];
        wgetnstr(stdscr, kb, 511);
        std::string key = std::string(kb);
        if (key.empty()) {
            noecho(); curs_set(0); return;
        }
        // if exists -> ask to confirm overwrite value
        if (conf.keyIndex.count(key)) {
            mvprintw(y, 0, "Key exists. Press Enter to edit its value.");
            wgetnstr(stdscr, kb, 511); // dummy wait
            // let editValue handle it by finding index in kv
            // rebuild kv
            kv = conf.kvList();
            for (size_t i=0;i<kv.size();++i) if (kv[i].first==key) { selected = i; break; }
            noecho(); curs_set(0);
            return;
        }
        mvprintw(y, 0, "Value for %s: ", key.c_str());
        char vb[2048];
        wgetnstr(stdscr, vb, 2047);
        std::string val(vb);
        conf.set(key, val);
        kv = conf.kvList();
        // select the new key
        for (size_t i=0;i<kv.size();++i) if (kv[i].first==key) { selected = i; break; }
        noecho(); curs_set(0);
    }

    void deleteKey() {
        if (kv.empty()) return;
        const std::string &key = kv[selected].first;
        // confirm
        int h,w; getmaxyx(stdscr,h,w);
        int y = h - statusWinH - 1;
        move(y, 0); clrtoeol();
        mvprintw(y, 0, "Delete '%s'? (y/N): ", key.c_str());
        int c = getch();
        if (c == 'y' || c == 'Y') {
            conf.remove(key);
            kv = conf.kvList();
            if (selected >= kv.size() && !kv.empty()) selected = kv.size()-1;
        }
    }

    void saveFile() {
        // backup
        std::string bak = filepath + ".bak";
        std::ifstream f(filepath, std::ios::binary);
        if (f) {
            std::ofstream out(bak, std::ios::binary);
            out << f.rdbuf();
        }
        bool ok = conf.save(filepath);
        int h,w; getmaxyx(stdscr,h,w);
        int y = h - statusWinH - 1;
        move(y, 0); clrtoeol();
        if (ok) {
            mvprintw(y, 0, "Saved. Backup: %s", bak.c_str());
        } else {
            mvprintw(y, 0, "Save failed!");
        }
        getch(); // wait for key to show message
    }

    bool confirmQuit() {
        int h,w; getmaxyx(stdscr,h,w);
        int y = h - statusWinH - 1;
        move(y, 0); clrtoeol();
        mvprintw(y, 0, "Quit without saving? (y/N) ");
        int c = getch();
        if (c == 'y' || c == 'Y') return false; // stop running -> will exit loop
        return true;
    }
};

// ------------------ main ------------------

int main(int argc, char **argv) {
    if (argc < 2) {
        printf("Usage: %s config.ini\n", argv[0]);
        return 1;
    }
    std::string path = argv[1];
    Config conf;
    if (!conf.load(path)) {
        // if doesn't exist, create empty config structure
        // but still proceed
    }
    Tui tui(conf, path);
    tui.run();
    return 0;
}
