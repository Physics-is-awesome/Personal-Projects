// menu_and_config.cpp
// Compile: g++ -std=c++17 -O2 -lncurses -o menu_and_config menu_and_config.cpp
#include <ncurses.h>
#include <string>
#include <vector>
#include <map>
#include <fstream>
#include <sstream>
#include <algorithm>
#include <cctype>
#include <filesystem>

// ---------------- Config structures ----------------
struct Line {
    enum Type { COMMENT, BLANK, KV } type;
    std::string raw, key, value;
};

class Config {
public:
    std::vector<Line> lines;
    std::map<std::string,size_t> keyIndex;

    bool load(const std::string &path) {
        std::ifstream f(path);
        if(!f.is_open()) return false;
        lines.clear();
        std::string ln;
        size_t idx=0;
        while(std::getline(f,ln)) {
            Line L;
            if(isBlank(ln)) {L.type=Line::BLANK;L.raw=ln;}
            else if(isComment(ln)) {L.type=Line::COMMENT;L.raw=ln;}
            else {
                auto kv=splitKV(ln);
                L.type=Line::KV;L.key=kv.first;L.value=kv.second;
                keyIndex[L.key]=idx;
            }
            lines.push_back(L);
            ++idx;
        }
        return true;
    }

    bool save(const std::string &path) {
        std::ofstream f(path);
        if(!f.is_open()) return false;
        for(auto &L:lines){
            if(L.type==Line::KV) f<<L.key<<" = "<<L.value<<"\n";
            else f<<L.raw<<"\n";
        }
        return true;
    }

    std::vector<std::pair<std::string,std::string>> kvList() const {
        std::vector<std::pair<std::string,std::string>> out;
        for(auto &L:lines)
            if(L.type==Line::KV) out.push_back({L.key,L.value});
        return out;
    }

    bool set(const std::string &k,const std::string &v){
        auto it=keyIndex.find(k);
        if(it!=keyIndex.end()){lines[it->second].value=v;return true;}
        Line L;L.type=Line::KV;L.key=k;L.value=v;
        keyIndex[k]=lines.size();lines.push_back(L);
        return true;
    }

    bool remove(const std::string &k){
        auto it=keyIndex.find(k);
        if(it==keyIndex.end())return false;
        lines.erase(lines.begin()+it->second);
        rebuildIndex();return true;
    }

private:
    void rebuildIndex(){
        keyIndex.clear();
        for(size_t i=0;i<lines.size();++i)
            if(lines[i].type==Line::KV) keyIndex[lines[i].key]=i;
    }
    static bool isBlank(const std::string&s){for(char c:s)if(!isspace((unsigned char)c))return false;return true;}
    static bool isComment(const std::string&s){for(char c:s){if(isspace((unsigned char)c))continue;return c=='#'||c==';';}return false;}
    static std::pair<std::string,std::string> splitKV(const std::string&s){
        size_t eq=s.find('=');
        auto trim=[](std::string t){size_t l=t.find_first_not_of(" \t\r\n"),r=t.find_last_not_of(" \t\r\n");if(l==std::string::npos)return std::string();return t.substr(l,r-l+1);};
        if(eq==std::string::npos) return {trim(s),""};
        return {trim(s.substr(0,eq)),trim(s.substr(eq+1))};
    }
};

// ------------- Config TUI runner ----------------
int run_config_tui(const std::string& path){
    Config conf; conf.load(path);
    auto kv = conf.kvList();
    size_t selected=0, top=0;
    const int statusH=3;
    bool running=true;

    initscr(); cbreak(); noecho(); keypad(stdscr,TRUE); curs_set(0);
    while(running){
        clear();
        int h,w; getmaxyx(stdscr,h,w);
        mvprintw(0,0,"Config Editor - %s", path.c_str());
        int listH=h-statusH-1;
        if(selected<top) top=selected;
        while(top+listH<=selected && top+1<=selected) ++top;

        for(int i=0;i<listH && (size_t)i+top<kv.size();++i){
            size_t idx=top+i;
            std::string line=kv[idx].first+" = "+kv[idx].second;
            if(line.size()> (size_t)w-2) line=line.substr(0,w-5)+"...";
            if(idx==selected){attron(A_REVERSE);mvprintw(1+i,0,"%s",line.c_str());attroff(A_REVERSE);}
            else mvprintw(1+i,0,"%s",line.c_str());
        }
        mvhline(h-statusH,0,'-',w);
        mvprintw(h-statusH+1,0,"Enter:edit  a:add  d:delete  s:save  q:quit");
        refresh();

        int ch=getch();
        switch(ch){
            case 'q': running=false; break;
            case KEY_UP: if(selected>0)--selected; break;
            case KEY_DOWN: if(selected+1<kv.size())++selected; break;
            case 10: { // edit
                echo(); curs_set(1);
                mvprintw(h-statusH+2,0,"Value for %s: ", kv[selected].first.c_str());
                char buf[1024]; getnstr(buf,1023);
                std::string val(buf);
                conf.set(kv[selected].first,val);
                kv=conf.kvList();
                noecho(); curs_set(0);
            } break;
            case 'a': {
                echo(); curs_set(1);
                mvprintw(h-statusH+2,0,"New key: "); char k[256]; getnstr(k,255);
                mvprintw(h-statusH+2,0,"Value:    "); char v[256]; getnstr(v,255);
                conf.set(k,v); kv=conf.kvList(); noecho(); curs_set(0);
            } break;
            case 'd': if(!kv.empty()){conf.remove(kv[selected].first);kv=conf.kvList();if(selected>=kv.size()&&selected>0)--selected;}break;
            case 's': conf.save(path); break;
        }
    }
    endwin();
    return 0;
}

// ---------------- Menu ----------------
void print_menu(WINDOW* w,int hl,const std::vector<std::string>& items){
    box(w,0,0);
    for(size_t i=0;i<items.size();++i){
        if((int)i+1==hl) wattron(w,A_REVERSE);
        mvwprintw(w,i+1,2,"%s",items[i].c_str());
        wattroff(w,A_REVERSE);
    }
    wrefresh(w);
}

int main(){
    initscr(); noecho(); cbreak(); curs_set(0); keypad(stdscr,TRUE);
    int h=10,w=40,y=4,x=10;
    WINDOW* win=newwin(h,w,y,x); keypad(win,TRUE);

    std::vector<std::string> choices={"Edit Configuration","Run Simulation","Generate Fortran Code","View Results","Exit"};
    int highlight=1,choice=0;
    namespace fs = std::filesystem;

    fs::path config_path = fs::path("config") / "config.cfg";

    while(true){
        mvprintw(1,10,"Use ↑↓ and Enter to select.");
        refresh();
        print_menu(win,highlight,choices);
        int c=wgetch(win);
        switch(c){
            case KEY_UP: if(highlight>1)--highlight; break;
            case KEY_DOWN: if(highlight<(int)choices.size())++highlight; break;
            case 10: choice=highlight; break;
        }
        if(choice){
            std::string sel=choices[choice-1];
            if(sel=="Exit") break;
            if(sel=="Edit Configuration"){
                endwin();          // leave menu mode
                run_config_tui(config_path.string());
                // back to menu mode
                initscr(); noecho(); cbreak(); curs_set(0); keypad(stdscr,TRUE);
                win=newwin(h,w,y,x); keypad(win,TRUE);
            } else {
                clear(); mvprintw(5,10,"Selected: %s", sel.c_str()); refresh();
                getch();
            }
            choice=0;
        }
    }
    delwin(win); endwin(); return 0;
}
