import numpy as np
import torch
import torch.nn as nn
import torch.optim as optim
from torch.distributions.bernoulli import Bernoulli
import pygame
import os
import argparse

# Game class (previously in Ast.py)
class Game:
    def __init__(self):
        pygame.init()
        self.WIDTH = 800
        self.HEIGHT = 600
        self.screen = pygame.display.set_mode((self.WIDTH, self.HEIGHT))
        self.clock = pygame.time.Clock()
        self.BLACK = (0, 0, 0)
        self.keys = set()
        self.ship = {"x": 400, "y": 300, "dx": 0, "dy": 0, "angle": 0, "radius": 10, "thrusting": False}
        self.bullets = []
        self.enemy_bullets = []
        self.asteroids = []
        self.ufo = None
        self.black_holes = []
        self.dark_matter_clouds = []
        self.particles = []
        self.lives = 3
        self.score = 0
        self.level = 1
        self.game_state = "playing"
        self.current_mode = "relativistic"
        self.SPEED_OF_LIGHT = 10
        self.SHIP_SPEED = 0.5
        self.ROTATION_SPEED = 5
        self.FRICTION = 0.99
        self.shoot_cooldown = 0
        self.shot_reset_timer = 60
        self.shot_count = 0
        self.ufo_spawn_timer = 100
        self.ASTEROID_SIZES = [30, 20, 10]
        self.GAME_MODES = {
            "relativistic": {
                "ufo_spawn_min": 50,
                "ufo_spawn_max": 200,
                "asteroids_per_wave": lambda level: 4 + level,
                "relativistic": True
            }
        }
        self.last_outputs = None
        self.last_reward = 0

    def reset_game(self):
        self.ship = {"x": 400, "y": 300, "dx": 0, "dy": 0, "angle": 0, "radius": 10, "thrusting": False}
        self.bullets = []
        self.enemy_bullets = []
        self.asteroids = []
        self.ufo = None
        self.black_holes = []
        self.particles = []
        self.lives = 3
        self.score = 0
        self.level = 1
        self.game_state = "playing"
        self.ufo_spawn_timer = 100
        self.keys = set()

    def update_bullets(self):
        for bullet in self.bullets[:]:
            bullet["x"] += bullet["dx"]
            bullet["y"] += bullet["dy"]
            bullet["life"] -= 1
            if bullet["life"] <= 0:
                self.bullets.remove(bullet)
        for bullet in self.enemy_bullets[:]:
            bullet["x"] += bullet["dx"]
            bullet["y"] += bullet["dy"]
            bullet["life"] -= 1
            if bullet["life"] <= 0:
                self.enemy_bullets.remove(bullet)

    def shoot_bullet(self):
        self.bullets.append({
            "x": self.ship["x"],
            "y": self.ship["y"],
            "dx": np.cos(np.radians(self.ship["angle"])) * 10,
            "dy": -np.sin(np.radians(self.ship["angle"])) * 10,
            "life": 30
        })
        self.shoot_cooldown = 10
        self.shot_count += 1
        self.shot_reset_timer = 60

    def update(self):
        print(f"Processing keys: {self.keys}")  # Debug AI actions
        if hasattr(self, 'apply_gravity'):
            self.apply_gravity()
        if hasattr(self, 'apply_relativistic_effects') and self.apply_relativistic_effects(self.ship):
            if pygame.K_LEFT in self.keys:
                self.ship["angle"] += self.ROTATION_SPEED
            if pygame.K_RIGHT in self.keys:
                self.ship["angle"] -= self.ROTATION_SPEED
            self.ship["thrusting"] = pygame.K_UP in self.keys
            if self.ship["thrusting"]:
                self.ship["dx"] += np.cos(np.radians(self.ship["angle"])) * self.SHIP_SPEED
                self.ship["dy"] -= np.sin(np.radians(self.ship["angle"])) * self.SHIP_SPEED
            if pygame.K_SPACE in self.keys and self.shoot_cooldown <= 0:
                self.shoot_bullet()
        self.ship["x"] += self.ship["dx"]
        self.ship["y"] += self.ship["dy"]
        self.ship["dx"] *= self.FRICTION
        self.ship["dy"] *= self.FRICTION
        self.ship["x"] %= self.WIDTH
        self.ship["y"] %= self.HEIGHT
        self.update_bullets()
        self.update_asteroids()
        self.update_ufo()
        self.ufo_spawn_timer -= 1
        if self.ufo_spawn_timer <= 0 and self.ufo is None:
            self.spawn_ufo()
            self.ufo_spawn_timer = np.random.randint(
                self.GAME_MODES[self.current_mode]["ufo_spawn_min"],
                self.GAME_MODES[self.current_mode]["ufo_spawn_max"]
            )
        for cloud in self.dark_matter_clouds:
            cloud["x"] += cloud["dx"]
            cloud["y"] += cloud["dy"]
            cloud["x"] %= self.WIDTH
            cloud["y"] %= self.HEIGHT
        for bh in self.black_holes:
            bh["x"] += bh["dx"]
            bh["y"] += bh["dy"]
            bh["x"] %= self.WIDTH
            bh["y"] %= self.HEIGHT
        if len(self.asteroids) == 0:
            self.level += 1
            asteroid_count = self.GAME_MODES[self.current_mode]["asteroids_per_wave"](self.level)
            for _ in range(asteroid_count):
                self.spawn_asteroid(self.ASTEROID_SIZES[0])
            if self.GAME_MODES[self.current_mode].get("relativistic", False):
                self.spawn_black_holes()
        for particle in self.particles[:]:
            particle["x"] += particle["dx"]
            particle["y"] += particle["dy"]
            particle["life"] -= 1
            if particle["life"] <= 0:
                self.particles.remove(particle)

    def apply_gravity(self):
        pass  # Implement if needed

    def apply_relativistic_effects(self, obj):
        return True  # Implement if needed

    def update_asteroids(self):
        for asteroid in self.asteroids[:]:
            asteroid["x"] += asteroid["dx"]
            asteroid["y"] += asteroid["dy"]
            asteroid["x"] %= self.WIDTH
            asteroid["y"] %= self.HEIGHT

    def update_ufo(self):
        if self.ufo:
            self.ufo["x"] += self.ufo["dx"]
            self.ufo["y"] += self.ufo["dy"]
            self.ufo["x"] %= self.WIDTH
            self.ufo["y"] %= self.HEIGHT

    def spawn_ufo(self):
        self.ufo = {"x": np.random.randint(0, self.WIDTH), "y": np.random.randint(0, self.HEIGHT),
                    "dx": np.random.uniform(-2, 2), "dy": np.random.uniform(-2, 2), "type": "small"}

    def spawn_asteroid(self, size):
        self.asteroids.append({
            "x": np.random.randint(0, self.WIDTH),
            "y": np.random.randint(0, self.HEIGHT),
            "dx": np.random.uniform(-1, 1),
            "dy": np.random.uniform(-1, 1),
            "radius": size
        })

    def spawn_black_holes(self):
        self.black_holes.append({
            "x": np.random.randint(0, self.WIDTH),
            "y": np.random.randint(0, self.HEIGHT),
            "dx": 0,
            "dy": 0,
            "radius": 50
        })

    def draw_objects(self):
        self.screen.fill(self.BLACK)
        pygame.draw.circle(self.screen, (255, 255, 255), (int(self.ship["x"]), int(self.ship["y"])), self.ship["radius"])
        for bullet in self.bullets:
            pygame.draw.circle(self.screen, (0, 255, 0), (int(bullet["x"]), int(bullet["y"])), 2)
        for bullet in self.enemy_bullets:
            pygame.draw.circle(self.screen, (255, 0, 0), (int(bullet["x"]), int(bullet["y"])), 2)
        for asteroid in self.asteroids:
            pygame.draw.circle(self.screen, (128, 128, 128), (int(asteroid["x"]), int(asteroid["y"])), asteroid["radius"])
        if self.ufo:
            pygame.draw.circle(self.screen, (0, 0, 255), (int(self.ufo["x"]), int(self.ufo["y"])), 15)
        for bh in self.black_holes:
            pygame.draw.circle(self.screen, (0, 0, 0), (int(bh["x"]), int(bh["y"])), bh["radius"])
        for particle in self.particles:
            pygame.draw.circle(self.screen, (255, 255, 0), (int(particle["x"]), int(particle["y"])), 1)

# Neural Network for Actor-Critic
class ActorCritic(nn.Module):
    def __init__(self, input_size=21, hidden_size=64, output_size=4):
        super(ActorCritic, self).__init__()
        self.device = torch.device("cuda" if torch.cuda.is_available() else "cpu")
        self.shared = nn.Sequential(
            nn.Linear(input_size, hidden_size),
            nn.ReLU(),
            nn.Linear(hidden_size, hidden_size),
            nn.ReLU()
        )
        self.actor = nn.Linear(hidden_size, output_size)
        self.critic = nn.Linear(hidden_size, 1)
        self.to(self.device)
    
    def forward(self, x):
        x = torch.FloatTensor(x).to(self.device)
        shared = self.shared(x)
        action_logits = self.actor(shared)
        value = self.critic(shared)
        return action_logits, value

# PPO Agent
class PPOAgent:
    def __init__(self, input_size=21, hidden_size=64, output_size=4, lr=3e-4, clip_eps=0.2, gae_lambda=0.95, gamma=0.99):
        self.device = torch.device("cuda" if torch.cuda.is_available() else "cpu")
        self.model = ActorCritic(input_size, hidden_size, output_size).to(self.device)
        self.optimizer = optim.Adam(self.model.parameters(), lr=lr)
        self.clip_eps = clip_eps
        self.gae_lambda = gae_lambda
        self.gamma = gamma
        self.input_size = input_size
    
    def get_game_state(self, game):
        state = [
            game.ship["x"] / game.WIDTH,
            game.ship["y"] / game.HEIGHT,
            game.ship["dx"] / game.SPEED_OF_LIGHT,
            game.ship["dy"] / game.SPEED_OF_LIGHT,
            game.ship["angle"] / 360
        ]
        if game.asteroids:
            distances = [np.hypot(a["x"] - game.ship["x"], a["y"] - game.ship["y"]) for a in game.asteroids]
            nearest = game.asteroids[np.argmin(distances)]
            dx = nearest["x"] - game.ship["x"]
            dy = nearest["y"] - game.ship["y"]
            if dx > game.WIDTH / 2:
                dx -= game.WIDTH
            elif dx < -game.WIDTH / 2:
                dx += game.WIDTH
            if dy > game.HEIGHT / 2:
                dy -= game.HEIGHT
            elif dy < -game.HEIGHT / 2:
                dy += game.HEIGHT
            dist = np.hypot(dx, dy)
            angle = np.arctan2(dy, dx) / (2 * np.pi)
            state.extend([
                dist / game.WIDTH,
                nearest["dx"] / game.SPEED_OF_LIGHT,
                nearest["dy"] / game.SPEED_OF_LIGHT,
                angle,
                game.ASTEROID_SIZES.index(nearest["radius"]) / 2 if nearest["radius"] in game.ASTEROID_SIZES else 0
            ])
        else:
            state.extend([0] * 5)
        if game.ufo:
            dx = game.ufo["x"] - game.ship["x"]
            dy = game.ufo["y"] - game.ship["y"]
            if dx > game.WIDTH / 2:
                dx -= game.WIDTH
            elif dx < -game.WIDTH / 2:
                dx += game.WIDTH
            if dy > game.HEIGHT / 2:
                dy -= game.HEIGHT
            elif dy < -game.HEIGHT / 2:
                dy += game.HEIGHT
            dist = np.hypot(dx, dy)
            angle = np.arctan2(dy, dx) / (2 * np.pi)
            state.extend([
                dist / game.WIDTH,
                game.ufo["dx"] / game.SPEED_OF_LIGHT,
                game.ufo["dy"] / game.SPEED_OF_LIGHT,
                angle,
                0 if game.ufo["type"] == "small" else 1
            ])
        else:
            state.extend([0] * 5)
        if game.black_holes:
            distances = [np.hypot(bh["x"] - game.ship["x"], bh["y"] - game.ship["y"]) for bh in game.black_holes]
            nearest = game.black_holes[np.argmin(distances)]
            dx = nearest["x"] - game.ship["x"]
            dy = nearest["y"] - game.ship["y"]
            if dx > game.WIDTH / 2:
                dx -= game.WIDTH
            elif dx < -game.WIDTH / 2:
                dx += game.WIDTH
            if dy > game.HEIGHT / 2:
                dy -= game.HEIGHT
            elif dy < -game.HEIGHT / 2:
                dy += game.HEIGHT
            dist = np.hypot(dx, dy)
            angle = np.arctan2(dy, dx) / (2 * np.pi)
            state.extend([
                dist / game.WIDTH,
                nearest["dx"] / game.SPEED_OF_LIGHT,
                nearest["dy"] / game.SPEED_OF_LIGHT,
                angle
            ])
        else:
            state.extend([0] * 4)
        state.extend([len(game.bullets) / 10, len(game.enemy_bullets) / 10])
        assert len(state) == 21, f"Expected 21 features, got {len(state)}"
        return state

    def collect_rollouts(self, game, episodes=10, max_steps=3600, headless=True):
        rollouts = []
        print(f"Starting collect_rollouts: episodes={episodes}, max_steps={max_steps}, headless={headless}")
        for ep in range(episodes):
            game.game_state = "playing"
            game.current_mode = "relativistic"
            print(f"Episode {ep+1}: Resetting game")
            game.reset_game()
            states, actions, log_probs, rewards, values, dones = [], [], [], [], [], []
            steps = 0
            prev_lives = game.lives
            prev_score = 0
            prev_ufo = None
            while game.game_state == "playing" and steps < max_steps:
                # Suppress user input
                pygame.event.clear()
                pygame.event.pump()
                
                try:
                    state = self.get_game_state(game)
                except Exception as e:
                    print(f"Error in get_game_state: {e}")
                    break
                
                state_tensor = torch.FloatTensor(state).to(self.device)
                with torch.no_grad():
                    action_logits, value = self.model(state_tensor)
                probs = torch.sigmoid(action_logits)
                dist = Bernoulli(probs)
                action = dist.sample()
                log_prob = dist.log_prob(action).sum()
                
                game.keys = set()
                if action[0] > 0.5:
                    game.keys.add(pygame.K_LEFT)
                if action[1] > 0.5:
                    game.keys.add(pygame.K_RIGHT)
                if action[2] > 0.5:
                    game.keys.add(pygame.K_UP)
                if action[3] > 0.5 and game.shoot_cooldown <= 0:
                    game.keys.add(pygame.K_SPACE)
                print(f"Step {steps}: AI actions: {game.keys}, Ship: x={game.ship['x']:.2f}, y={game.ship['y']:.2f}, angle={game.ship['angle']:.2f}, Bullets={len(game.bullets)}, Score={game.score}")
                
                game.last_outputs = probs.cpu().numpy().round(2)
                game.last_reward = 0
                
                try:
                    game.update()
                except Exception as e:
                    print(f"Error in game.update: {e}")
                    break
                
                game.shoot_cooldown = max(0, game.shoot_cooldown - 1)
                game.shot_reset_timer = max(0, game.shot_reset_timer - 1)
                if game.shot_reset_timer <= 0:
                    game.shot_count = 0
                game.ufo_spawn_timer -= 1
                if game.ufo_spawn_timer <= 0 and game.ufo is None:
                    game.spawn_ufo()
                    game.ufo_spawn_timer = np.random.randint(
                        game.GAME_MODES[game.current_mode]["ufo_spawn_min"],
                        game.GAME_MODES[game.current_mode]["ufo_spawn_max"]
                    )
                for cloud in game.dark_matter_clouds:
                    cloud["x"] += cloud["dx"]
                    cloud["y"] += cloud["dy"]
                    cloud["x"] %= game.WIDTH
                    cloud["y"] %= game.HEIGHT
                for bh in game.black_holes:
                    bh["x"] += bh["dx"]
                    bh["y"] += bh["dy"]
                    bh["x"] %= game.WIDTH
                    bh["y"] %= game.HEIGHT
                if len(game.asteroids) == 0:
                    game.level += 1
                    asteroid_count = game.GAME_MODES[game.current_mode]["asteroids_per_wave"](game.level)
                    for _ in range(asteroid_count):
                        game.spawn_asteroid(game.ASTEROID_SIZES[0])
                    if game.GAME_MODES[game.current_mode].get("relativistic", False):
                        game.spawn_black_holes()
                for particle in game.particles[:]:
                    particle["x"] += particle["dx"]
                    particle["y"] += particle["dy"]
                    particle["life"] -= 1
                    if particle["life"] <= 0:
                        game.particles.remove(particle)
                
                reward = 1
                if game.ship["thrusting"]:
                    reward -= 10
                if game.lives < prev_lives:
                    reward -= 1000
                if any(np.hypot(game.ship["x"] - bh["x"], game.ship["y"] - bh["y"]) < bh["radius"] + game.ship["radius"] for bh in game.black_holes):
                    reward -= 5000
                reward += (game.score - prev_score)
                if game.ufo is None and prev_ufo is not None:
                    reward += 500
                prev_lives = game.lives
                prev_score = game.score
                prev_ufo = game.ufo
                game.last_reward = reward
                
                states.append(state)
                actions.append(action.cpu().numpy())
                log_probs.append(log_prob.cpu().item())
                rewards.append(reward)
                values.append(value.cpu().item())
                dones.append(game.game_state != "playing")
                
                steps += 1
                if not headless and game.screen is not None:
                    print(f"Rendering: Step {steps}")
                    game.screen.fill(game.BLACK)
                    game.draw_objects()
                    pygame.display.flip()
                    game.clock.tick(30)
                
                if game.lives <= 0 and game.current_mode != "time_attack":
                    game.game_state = "game_over"
            
            if steps < max_steps and game.game_state == "playing":
                with torch.no_grad():
                    _, value = self.model(torch.FloatTensor(self.get_game_state(game)).to(self.device))
                values.append(value.cpu().item())
            else:
                values.append(0)
            
            rollouts.append((states, actions, log_probs, rewards, values, dones))
            print(f"Episode {ep+1}: Score={game.score}, Steps={steps}, Reward={sum(rewards):.2f}")
        
        return rollouts

    def compute_gae(self, rewards, values, dones):
        advantages = []
        returns = []
        gae = 0
        for i in reversed(range(len(rewards))):
            delta = rewards[i] + self.gamma * values[i + 1] * (1 - dones[i]) - values[i]
            gae = delta + self.gamma * self.gae_lambda * (1 - dones[i]) * gae
            advantages.insert(0, gae)
            returns.insert(0, gae + values[i])
        return advantages, returns

    def update(self, rollouts, epochs=10, batch_size=64):
        states = []
        actions = []
        old_log_probs = []
        advantages = []
        returns = []
        for rollout in rollouts:
            rollout_states, rollout_actions, rollout_log_probs, rollout_rewards, rollout_values, rollout_dones = rollout
            rollout_advantages, rollout_returns = self.compute_gae(rollout_rewards, rollout_values, rollout_dones)
            states.extend(rollout_states)
            actions.extend(rollout_actions)
            old_log_probs.extend(rollout_log_probs)
            advantages.extend(rollout_advantages)
            returns.extend(rollout_returns)
        
        states = torch.FloatTensor(states).to(self.device)
        actions = torch.FloatTensor(actions).to(self.device)
        old_log_probs = torch.FloatTensor(old_log_probs).to(self.device)
        advantages = torch.FloatTensor(advantages).to(self.device)
        advantages = (advantages - advantages.mean()) / (advantages.std() + 1e-8)
        returns = torch.FloatTensor(returns).to(self.device)
        
        for _ in range(epochs):
            for i in range(0, len(states), batch_size):
                batch_indices = slice(i, i + batch_size)
                batch_states = states[batch_indices]
                batch_actions = actions[batch_indices]
                batch_old_log_probs = old_log_probs[batch_indices]
                batch_advantages = advantages[batch_indices]
                batch_returns = returns[batch_indices]
                
                action_logits, value = self.model(batch_states)
                dist = Bernoulli(torch.sigmoid(action_logits))
                new_log_probs = dist.log_prob(batch_actions).sum(dim=1)
                entropy = dist.entropy().mean()
                
                ratio = torch.exp(new_log_probs - batch_old_log_probs)
                surr1 = ratio * batch_advantages
                surr2 = torch.clamp(ratio, 1 - self.clip_eps, 1 + self.clip_eps) * batch_advantages
                actor_loss = -torch.min(surr1, surr2).mean()
                critic_loss = nn.MSELoss()(value.squeeze(), batch_returns)
                loss = actor_loss + 0.5 * critic_loss - 0.01 * entropy
                
                self.optimizer.zero_grad()
                loss.backward()
                self.optimizer.step()
        print("Completed PPO update")

    def save_model(self, path="/home/ajc/Personal-Projects/Random stuff/new_ppo_model.pth"):
        torch.save(self.model.state_dict(), path)
        print(f"Saved model to {path}")

    def load_model(self, path="/home/ajc/Personal-Projects/Random stuff/new_ppo_model.pth"):
        if os.path.exists(path):
            self.model.load_state_dict(torch.load(path))
            print(f"Loaded model from {path}")

# Main
if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="PPO for Asteroids AI")
    parser.add_argument("--test", action="store_true", help="Run test mode with trained model")
    parser.add_argument("--manual", action="store_true", help="Run in manual play mode")
    args = parser.parse_args()
    
    try:
        pygame.init()
        game = Game()
        
        if args.manual:
            print("Running in manual mode...")
            game.game_state = "playing"
            game.current_mode = "relativistic"
            game.reset_game()
            while game.game_state == "playing":
                for event in pygame.event.get():
                    if event.type == pygame.QUIT:
                        game.game_state = "game_over"
                    elif event.type == pygame.KEYDOWN:
                        game.keys.add(event.key)
                    elif event.type == pygame.KEYUP:
                        game.keys.discard(event.key)
                game.update()
                if game.screen is not None:
                    game.screen.fill(game.BLACK)
                    game.draw_objects()
                    pygame.display.flip()
                    game.clock.tick(30)
        else:
            agent = PPOAgent()
            agent.load_model()
            
            if args.test:
                print("Testing trained agent...")
                rollouts = agent.collect_rollouts(game, episodes=1, headless=False, max_steps=7200)
                print("Completed test run")
            else:
                iterations = 10  # Reduced for debugging
                for i in range(iterations):
                    print(f"Iteration {i+1}/{iterations}")
                    headless = i % 2 != 0  # Visualize every other iteration
                    rollouts = agent.collect_rollouts(game, episodes=5, headless=headless, max_steps=3600)
                    agent.update(rollouts)
                    total_rewards = [sum(rollout[3]) for rollout in rollouts]
                    print(f"Average Reward: {sum(total_rewards)/len(total_rewards):.2f}, Max: {max(total_rewards):.2f}")
                    agent.save_model()
                print("Testing with best model...")
                agent.collect_rollouts(game, episodes=1, headless=False, max_steps=7200)
    except Exception as e:
        print(f"Error occurred: {e}")
        import traceback
        traceback.print_exc()
    finally:
        pygame.quit()
