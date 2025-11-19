// main.cpp
// Simple Asteroids-like game using SFML
// Controls: Left/Right rotate, Up thrust, Space shoot, Esc quit

#include <SFML/Graphics.hpp>
#include <cmath>
#include <vector>
#include <random>

constexpr float PI = 3.14159265359f;

float length(const sf::Vector2f& v) {
    return std::sqrt(v.x * v.x + v.y * v.y);
}
sf::Vector2f normalize(const sf::Vector2f& v) {
    float len = length(v);
    return (len > 0.f) ? sf::Vector2f(v.x / len, v.y / len) : sf::Vector2f(0.f, 0.f);
}
sf::Vector2f rotateVec(const sf::Vector2f& v, float angleRad) {
    float c = std::cos(angleRad), s = std::sin(angleRad);
    return { v.x * c - v.y * s, v.x * s + v.y * c };
}
sf::Vector2f wrap(const sf::Vector2f& pos, const sf::Vector2u& size) {
    sf::Vector2f p = pos;
    if (p.x < 0) p.x += size.x;
    if (p.x >= size.x) p.x -= size.x;
    if (p.y < 0) p.y += size.y;
    if (p.y >= size.y) p.y -= size.y;
    return p;
}

struct Bullet {
    sf::Vector2f pos;
    sf::Vector2f vel;
    float life = 1.2f; // seconds
    float radius = 2.f;

    void update(float dt, sf::Vector2u size) {
        pos += vel * dt;
        pos = wrap(pos, size);
        life -= dt;
    }
    void draw(sf::RenderWindow& win) const {
        sf::CircleShape c(radius);
        c.setOrigin(radius, radius);
        c.setPosition(pos);
        c.setFillColor(sf::Color::White);
        win.draw(c);
    }
};

struct Asteroid {
    sf::Vector2f pos;
    sf::Vector2f vel;
    float radius;
    float rotSpeed;
    float angle = 0.f;
    std::vector<sf::Vector2f> shape; // polygon points (unit circle-ish)

    static std::vector<sf::Vector2f> randomShape(std::mt19937& rng, int verts = 10) {
        std::uniform_real_distribution<float> jitter(0.75f, 1.25f);
        std::vector<sf::Vector2f> pts;
        pts.reserve(verts);
        for (int i = 0; i < verts; ++i) {
            float t = (2 * PI * i) / verts;
            float r = jitter(rng);
            pts.push_back({ std::cos(t) * r, std::sin(t) * r });
        }
        return pts;
    }

    void update(float dt, sf::Vector2u size) {
        pos += vel * dt;
        pos = wrap(pos, size);
        angle += rotSpeed * dt;
    }
    void draw(sf::RenderWindow& win) const {
        sf::ConvexShape poly;
        poly.setPointCount(shape.size());
        for (size_t i = 0; i < shape.size(); ++i) {
            sf::Vector2f p = rotateVec(shape[i] * radius, angle);
            poly.setPoint(i, p);
        }
        poly.setPosition(pos);
        poly.setFillColor(sf::Color(30, 30, 30));
        poly.setOutlineColor(sf::Color::White);
        poly.setOutlineThickness(2.f);
        win.draw(poly);
    }
};

struct Ship {
    sf::Vector2f pos;
    sf::Vector2f vel;
    float angle = 0.f; // radians
    float radius = 12.f;

    void update(float dt, const sf::Vector2u& size, bool left, bool right, bool thrust) {
        constexpr float rotSpeed = 2.5f;     // rad/s
        constexpr float accel = 220.f;       // px/s^2
        constexpr float drag = 0.995f;       // simple damping

        if (left)  angle -= rotSpeed * dt;
        if (right) angle += rotSpeed * dt;

        if (thrust) {
            sf::Vector2f forward = { std::cos(angle - PI/2), std::sin(angle - PI/2) };
            vel += forward * accel * dt;
        }

        vel *= drag;
        pos += vel * dt;
        pos = wrap(pos, size);
    }

    Bullet shoot() const {
        sf::Vector2f forward = { std::cos(angle - PI/2), std::sin(angle - PI/2) };
        Bullet b;
        b.pos = pos + forward * (radius + 4.f);
        b.vel = vel + forward * 420.f;
        return b;
    }

    void draw(sf::RenderWindow& win, bool thrusting) const {
        // Ship triangle points in local space pointing up (-y)
        sf::Vector2f p1 = rotateVec({0, -radius}, angle);
        sf::Vector2f p2 = rotateVec({radius * 0.8f, radius}, angle);
        sf::Vector2f p3 = rotateVec({-radius * 0.8f, radius}, angle);

        sf::ConvexShape tri;
        tri.setPointCount(3);
        tri.setPoint(0, p1);
        tri.setPoint(1, p2);
        tri.setPoint(2, p3);
        tri.setPosition(pos);
        tri.setFillColor(sf::Color::Transparent);
        tri.setOutlineColor(sf::Color::White);
        tri.setOutlineThickness(2.f);
        win.draw(tri);

        if (thrusting) {
            sf::ConvexShape flame;
            flame.setPointCount(3);
            sf::Vector2f f1 = rotateVec({0, radius + 6}, angle);
            sf::Vector2f f2 = rotateVec({-radius * 0.4f, radius}, angle);
            sf::Vector2f f3 = rotateVec({radius * 0.4f, radius}, angle);
            flame.setPoint(0, f1);
            flame.setPoint(1, f2);
            flame.setPoint(2, f3);
            flame.setPosition(pos);
            flame.setFillColor(sf::Color(255, 120, 0));
            win.draw(flame);
        }
    }
};

bool circleHit(const sf::Vector2f& a, float ra, const sf::Vector2f& b, float rb) {
    sf::Vector2f d = a - b;
    float r = ra + rb;
    return (d.x * d.x + d.y * d.y) <= (r * r);
}

int main() {
    sf::Vector2u screenSize(960, 720);
    sf::RenderWindow window(sf::VideoMode(screenSize.x, screenSize.y), "Asteroids (SFML)");
    window.setVerticalSyncEnabled(true);

    // Random engine
    std::random_device rd;
    std::mt19937 rng(rd());
    std::uniform_real_distribution<float> angleDist(0.f, 2.f * PI);
    std::uniform_real_distribution<float> speedDist(40.f, 110.f);
    std::uniform_real_distribution<float> radDist(18.f, 44.f);

    // Game state
    Ship ship;
    ship.pos = { screenSize.x * 0.5f, screenSize.y * 0.5f };
    ship.vel = { 0.f, 0.f };
    ship.angle = 0.f;

    std::vector<Bullet> bullets;
    std::vector<Asteroid> asteroids;

    auto spawnAsteroid = [&](float radius = -1.f, const sf::Vector2f* atPos = nullptr) {
        Asteroid a;
        a.radius = (radius < 0.f) ? radDist(rng) : radius;
        a.pos = atPos ? *atPos : sf::Vector2f(
            (rng() % screenSize.x),
            (rng() % screenSize.y)
        );
        float ang = angleDist(rng);
        a.vel = { std::cos(ang) * speedDist(rng), std::sin(ang) * speedDist(rng) };
        a.rotSpeed = (rng() % 2 ? 1 : -1) * (0.3f + (rng() % 100) / 500.f);
        a.shape = Asteroid::randomShape(rng, 10);
        asteroids.push_back(a);
    };

    // Initial field
    for (int i = 0; i < 6; ++i) spawnAsteroid();

    sf::Font font;
    // Try to load a system font; if it fails, we’ll use shape-only UI
    bool fontLoaded = font.loadFromFile("/System/Library/Fonts/Supplemental/Arial.ttf")
                   || font.loadFromFile("C:\\Windows\\Fonts\\arial.ttf")
                   || font.loadFromFile("arial.ttf");

    int score = 0;
    int lives = 3;
    float shootCooldown = 0.f;
    const float shootDelay = 0.18f;
    bool gameOver = false;

    sf::Clock clock;
    while (window.isOpen()) {
        sf::Event e;
        while (window.pollEvent(e)) {
            if (e.type == sf::Event::Closed) window.close();
        }

        float dt = clock.restart().asSeconds();
        if (dt > 0.03f) dt = 0.03f; // simple clamp

        // Input
        bool left = sf::Keyboard::isKeyPressed(sf::Keyboard::Left);
        bool right = sf::Keyboard::isKeyPressed(sf::Keyboard::Right);
        bool thrust = sf::Keyboard::isKeyPressed(sf::Keyboard::Up);
        bool shootPressed = sf::Keyboard::isKeyPressed(sf::Keyboard::Space);
        if (sf::Keyboard::isKeyPressed(sf::Keyboard::Escape)) window.close();

        if (!gameOver) {
            ship.update(dt, screenSize, left, right, thrust);

            // Shooting
            shootCooldown -= dt;
            if (shootPressed && shootCooldown <= 0.f) {
                bullets.push_back(ship.shoot());
                shootCooldown = shootDelay;
            }

            // Update bullets
            for (auto& b : bullets) b.update(dt, screenSize);
            bullets.erase(
                std::remove_if(bullets.begin(), bullets.end(),
                    [](const Bullet& b){ return b.life <= 0.f; }),
                bullets.end()
            );

            // Update asteroids
            for (auto& a : asteroids) a.update(dt, screenSize);

            // Collisions: bullets vs asteroids
            for (size_t i = 0; i < bullets.size(); ++i) {
                for (size_t j = 0; j < asteroids.size(); ++j) {
                    if (circleHit(bullets[i].pos, bullets[i].radius, asteroids[j].pos, asteroids[j].radius)) {
                        // Split asteroid
                        float newR = asteroids[j].radius * 0.55f;
                        sf::Vector2f hitPos = asteroids[j].pos;
                        asteroids.erase(asteroids.begin() + j);
                        bullets.erase(bullets.begin() + i);
                        score += 10;
                        if (newR > 12.f) {
                            spawnAsteroid(newR, &hitPos);
                            spawnAsteroid(newR, &hitPos);
                        }
                        goto afterBulletLoop;
                    }
                }
            }
            afterBulletLoop: ;

            // Collisions: ship vs asteroids
            for (const auto& a : asteroids) {
                if (circleHit(ship.pos, ship.radius * 0.9f, a.pos, a.radius)) {
                    lives -= 1;
                    // Reset ship
                    ship.pos = { screenSize.x * 0.5f, screenSize.y * 0.5f };
                    ship.vel = { 0.f, 0.f };
                    ship.angle = 0.f;
                    if (lives <= 0) {
                        gameOver = true;
                    }
                    break;
                }
            }

            // Refill field if cleared
            if (asteroids.empty()) {
                for (int i = 0; i < 6; ++i) spawnAsteroid();
            }
        }

        // Draw
        window.clear(sf::Color::Black);

        for (const auto& a : asteroids) a.draw(window);
        for (const auto& b : bullets) b.draw(window);
        ship.draw(window, thrust && !gameOver);

        // HUD
        if (fontLoaded) {
            sf::Text hud;
            hud.setFont(font);
            hud.setCharacterSize(18);
            hud.setFillColor(sf::Color::White);
            hud.setString("Score: " + std::to_string(score) + "   Lives: " + std::to_string(lives));
            hud.setPosition(12.f, 8.f);
            window.draw(hud);

            if (gameOver) {
                sf::Text over("GAME OVER", font, 42);
                over.setFillColor(sf::Color(255, 80, 80));
                over.setPosition(screenSize.x * 0.5f - 140.f, screenSize.y * 0.5f - 30.f);
                window.draw(over);
            }
        }

        window.display();
    }
    return 0;
}
