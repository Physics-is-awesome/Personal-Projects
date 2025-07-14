import numpy as np
import torch
import torch.nn as nn
import torch.optim as optim
from torch.distributions.bernoulli import Bernoulli
import importlib.util
import sys
import pygame
import os
import argparse

# Load Ast.py
spec = importlib.util.spec_from_file_location("Ast_AI_env", "/home/ajc/Personal-Projects/Random stuff/Ast_AI_env.py")
ast = importlib.util.module_from_spec(spec)
sys.modules["Ast"] = ast
spec.loader.exec_module(ast)

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
    
    def get_game_state(self, ast_module):
        ship = ast_module.ship
        asteroids = ast_module.asteroids
        ufo = ast_module.ufo
        black_holes = ast_module.black_holes
        bullets = ast_module.bullets
        enemy_bullets = ast_module.enemy_bullets
        
        state = [
            ship["x"] / ast_module.WIDTH,
            ship["y"] / ast_module.HEIGHT,
            ship["dx"] / ast_module.SPEED_OF_LIGHT,
            ship["dy"] / ast_module.SPEED_OF_LIGHT,
            ship["angle"] / 360
        ]
        if asteroids:
            distances = [np.hypot(a["x"] - ship["x"], a["y"] - ship["y"]) for a in asteroids]
            nearest = asteroids[np.argmin(distances)]
            dx = nearest["x"] - ship["x"]
            dy = nearest["y"] - ship["y"]
            if dx > ast_module.WIDTH / 2:
                dx -= ast_module.WIDTH
            elif dx < -ast_module.WIDTH / 2:
                dx += ast_module.WIDTH
            if dy > ast_module.HEIGHT / 2:
                dy -= ast_module.HEIGHT
            elif dy < -ast_module.HEIGHT / 2:
                dy += ast_module.HEIGHT
            dist = np.hypot(dx, dy)
            angle = np.arctan2(dy, dx) / (2 * np.pi)
            state.extend([
                dist / ast_module.WIDTH,
                nearest["dx"] / ast_module.SPEED_OF_LIGHT,
                nearest["dy"] / ast_module.SPEED_OF_LIGHT,
                angle,
                ast_module.ASTEROID_SIZES.index(nearest["radius"]) / 2
            ])
        else:
            state.extend([0] * 5)
        if ufo:
            dx = ufo["x"] - ship["x"]
            dy = ufo["y"] - ship["y"]
            if dx > ast_module.WIDTH / 2:
                dx -= ast_module.WIDTH
            elif dx < -ast_module.WIDTH / 2:
                dx += ast_module.WIDTH
            if dy > ast_module.HEIGHT / 2:
                dy -= ast_module.HEIGHT
            elif dy < -ast_module.HEIGHT / 2:
                dy += ast_module.HEIGHT
            dist = np.hypot(dx, dy)
            angle = np.arctan2(dy, dx) / (2 * np.pi)
            state.extend([
                dist / ast_module.WIDTH,
                ufo["dx"] / ast_module.SPEED_OF_LIGHT,
                ufo["dy"] / ast_module.SPEED_OF_LIGHT,
                angle,
                0 if ufo["type"] == "small" else 1
            ])
        else:
            state.extend([0] * 5)
        if black_holes:
            distances = [np.hypot(bh["x"] - ship["x"], bh["y"] - ship["y"]) for bh in black_holes]
            nearest = black_holes[np.argmin(distances)]
            dx = nearest["x"] - ship["x"]
            dy = nearest["y"] - ship["y"]
            if dx > ast_module.WIDTH / 2:
                dx -= ast_module.WIDTH
            elif dx < -ast_module.WIDTH / 2:
                dx += ast_module.WIDTH
            if dy > ast_module.HEIGHT / 2:
                dy -= ast_module.HEIGHT
            elif dy < -ast_module.HEIGHT / 2:
                dy += ast_module.HEIGHT
            dist = np.hypot(dx, dy)
            angle = np.arctan2(dy, dx) / (2 * np.pi)
            state.extend([
                dist / ast_module.WIDTH,
                nearest["dx"] / ast_module.SPEED_OF_LIGHT,
                nearest["dy"] / ast_module.SPEED_OF_LIGHT,
                angle
            ])
        else:
            state.extend([0] * 4)
        state.extend([len(bullets) / 10, len(enemy_bullets) / 10])
        assert len(state) == 21, f"Expected 21 features, got {len(state)}"
        return state

    def collect_rollouts(self, ast_module, episodes=10, max_steps=3600, headless=True):
        rollouts = []
        for ep in range(episodes):
            ast_module.game_state = "playing"
            ast_module.current_mode = "relativistic"
            ast_module.reset_game()
            states, actions, log_probs, rewards, values, dones = [], [], [], [], [], []
            steps = 0
            prev_lives = ast_module.lives
            prev_score = 0
            prev_ufo = None
            while ast_module.game_state == "playing" and steps < max_steps:
                # Clear user input to ensure AI control
                pygame.event.clear()
                
                state = self.get_game_state(ast_module)
                state_tensor = torch.FloatTensor(state).to(self.device)
                with torch.no_grad():
                    action_logits, value = self.model(state_tensor)
                probs = torch.sigmoid(action_logits)
                dist = Bernoulli(probs)
                action = dist.sample()
                log_prob = dist.log_prob(action).sum()
                
                ast_module.keys = set()
                if action[0] > 0.5:
                    ast_module.keys.add(pygame.K_LEFT)
                if action[1] > 0.5:
                    ast_module.keys.add(pygame.K_RIGHT)
                if action[2] > 0.5:
                    ast_module.keys.add(pygame.K_UP)
                if action[3] > 0.5 and ast_module.shoot_cooldown <= 0:
                    ast_module.keys.add(pygame.K_SPACE)
                print(f"AI actions: {ast_module.keys}, Ship: x={ast_module.ship['x']:.2f}, y={ast_module.ship['y']:.2f}, angle={ast_module.ship['angle']:.2f}")  # Enhanced debug
                
                ast_module.last_outputs = probs.cpu().numpy().round(2)
                ast_module.last_reward = 0
                
                # Call update methods safely
                if hasattr(ast_module, 'apply_gravity'):
                    ast_module.apply_gravity()
                if hasattr(ast_module, 'apply_relativistic_effects') and ast_module.apply_relativistic_effects(ast_module.ship):
                    if pygame.K_LEFT in ast_module.keys:
                        ast_module.ship["angle"] += ast_module.ROTATION_SPEED
                    if pygame.K_RIGHT in ast_module.keys:
                        ast_module.ship["angle"] -= ast_module.ROTATION_SPEED
                    ast_module.ship["thrusting"] = pygame.K_UP in ast_module.keys
                    if ast_module.ship["thrusting"]:
                        ast_module.ship["dx"] += np.cos(np.radians(ast_module.ship["angle"])) * ast_module.SHIP_SPEED
                        ast_module.ship["dy"] -= np.sin(np.radians(ast_module.ship["angle"])) * ast_module.SHIP_SPEED
                ast_module.ship["x"] += ast_module.ship["dx"]
                ast_module.ship["y"] += ast_module.ship["dy"]
                ast_module.ship["dx"] *= ast_module.FRICTION
                ast_module.ship["dy"] *= ast_module.FRICTION
                if not ast_module.GAME_MODES[ast_module.current_mode].get("arena", False):
                    ast_module.ship["x"] %= ast_module.WIDTH
                    ast_module.ship["y"] %= ast_module.HEIGHT
                
                ast_module.shoot_cooldown = max(0, ast_module.shoot_cooldown - 1)
                ast_module.shot_reset_timer = max(0, ast_module.shot_reset_timer - 1)
                if ast_module.shot_reset_timer <= 0:
                    ast_module.shot_count = 0
                if hasattr(ast_module, 'update_bullets'):
                    ast_module.update_bullets()
                if hasattr(ast_module, 'update_asteroids'):
                    ast_module.update_asteroids()
                if hasattr(ast_module, 'update_ufo'):
                    ast_module.update_ufo()
                ast_module.ufo_spawn_timer -= 1
                if ast_module.ufo_spawn_timer <= 0 and ast_module.ufo is None:
                    if hasattr(ast_module, 'spawn_ufo'):
                        ast_module.spawn_ufo()
                        ast_module.ufo_spawn_timer = np.random.randint(
                            ast_module.GAME_MODES[ast_module.current_mode]["ufo_spawn_min"],
                            ast_module.GAME_MODES[ast_module.current_mode]["ufo_spawn_max"]
                        )
                for cloud in ast_module.dark_matter_clouds:
                    cloud["x"] += cloud["dx"]
                    cloud["y"] += cloud["dy"]
                    cloud["x"] %= ast_module.WIDTH
                    cloud["y"] %= ast_module.HEIGHT
                for bh in ast_module.black_holes:
                    bh["x"] += bh["dx"]
                    bh["y"] += bh["dy"]
                    bh["x"] %= ast_module.WIDTH
                    bh["y"] %= ast_module.HEIGHT
                if len(ast_module.asteroids) == 0:
                    ast_module.level += 1
                    asteroid_count = ast_module.GAME_MODES[ast_module.current_mode]["asteroids_per_wave"](ast_module.level)
                    for _ in range(asteroid_count):
                        if hasattr(ast_module, 'spawn_asteroid'):
                            ast_module.spawn_asteroid(ast_module.ASTEROID_SIZES[0])
                    if ast_module.GAME_MODES[ast_module.current_mode].get("relativistic", False):
                        if hasattr(ast_module, 'spawn_black_holes'):
                            ast_module.spawn_black_holes()
                for particle in ast_module.particles[:]:
                    particle["x"] += particle["dx"]
                    particle["y"] += particle["dy"]
                    particle["life"] -= 1
                    if particle["life"] <= 0:
                        ast_module.particles.remove(particle)
                
                reward = 1
                if ast_module.ship["thrusting"]:
                    reward -= 10
                if ast_module.lives < prev_lives:
                    reward -= 1000
                if any(np.hypot(ast_module.ship["x"] - bh["x"], ast_module.ship["y"] - bh["y"]) < bh["radius"] + ast_module.ship["radius"] for bh in ast_module.black_holes):
                    reward -= 5000
                reward += (ast_module.score - prev_score)
                if ast_module.ufo is None and prev_ufo is not None:
                    reward += 500
                prev_lives = ast_module.lives
                prev_score = ast_module.score
                prev_ufo = ast_module.ufo
                ast_module.last_reward = reward
                
                states.append(state)
                actions.append(action.cpu().numpy())
                log_probs.append(log_prob.cpu().item())
                rewards.append(reward)
                values.append(value.cpu().item())
                dones.append(ast_module.game_state != "playing")
                
                steps += 1
                if not headless and hasattr(ast_module, 'screen') and ast_module.screen is not None:
                    print(f"Rendering: Screen initialized={hasattr(ast_module, 'screen') and ast_module.screen is not None}")
                    ast_module.screen.fill(ast_module.BLACK)
                    if hasattr(ast_module, 'draw_objects'):
                        ast_module.draw_objects()
                    pygame.display.flip()
                    ast_module.clock.tick(30)
                
                if ast_module.lives <= 0 and ast_module.current_mode != "time_attack":
                    ast_module.game_state = "game_over"
            
            if steps < max_steps and ast_module.game_state == "playing":
                with torch.no_grad():
                    _, value = self.model(torch.FloatTensor(self.get_game_state(ast_module)).to(self.device))
                values.append(value.cpu().item())
            else:
                values.append(0)
            
            rollouts.append((states, actions, log_probs, rewards, values, dones))
            print(f"Episode {ep+1}: Score={ast_module.score}, Steps={steps}, Reward={sum(rewards):.2f}")
        
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

    def save_model(self, path="/home/ajc/Personal-Projects/Random stuff/new_ppo_model.pth"):
        torch.save(self.model.state_dict(), path)
        print(f"Saved model to {path}")

    def load_model(self, path="/home/ajc/Personal-Projects/Random stuff/new_ppo_model.pth"):
        if os.path.exists(path):
            self.model.load_state_dict(torch.load(path))
            print(f"Loaded model from {path}")

# Main
if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="PPO for Ast_AI_env.py")
    parser.add_argument("--test", action="store_true", help="Run test mode with trained model")
    parser.add_argument("--manual", action="store_true", help="Run in manual play mode")
    args = parser.parse_args()
    
    try:
        pygame.init()
        # Instantiate Ast.py as a class if it's structured that way
        if isinstance(ast, type):
            ast_module = ast()
        else:
            ast_module = ast
        
        if args.manual:
            print("Running in manual mode...")
            ast_module.game_state = "playing"
            ast_module.current_mode = "relativistic"
            ast_module.reset_game()
            while ast_module.game_state == "playing":
                for event in pygame.event.get():
                    if event.type == pygame.QUIT:
                        ast_module.game_state = "game_over"
                    elif event.type == pygame.KEYDOWN:
                        ast_module.keys.add(event.key)
                    elif event.type == pygame.KEYUP:
                        ast_module.keys.discard(event.key)
                ast_module.update()
                if hasattr(ast_module, 'screen') and ast_module.screen is not None:
                    ast_module.screen.fill(ast_module.BLACK)
                    if hasattr(ast_module, 'draw_objects'):
                        ast_module.draw_objects()
                    pygame.display.flip()
                    ast_module.clock.tick(30)
        else:
            agent = PPOAgent()
            agent.load_model()
            
            if args.test:
                print("Testing trained agent...")
                agent.collect_rollouts(ast_module, episodes=1, headless=False, max_steps=7200)
            else:
                iterations = 1000
                for i in range(iterations):
                    print(f"Iteration {i+1}/{iterations}")
                    headless = i % 10 != 0
                    rollouts = agent.collect_rollouts(ast_module, episodes=10, headless=headless)
                    agent.update(rollouts)
                    total_rewards = [sum(rollout[3]) for rollout in rollouts]
                    print(f"Average Reward: {sum(total_rewards)/len(total_rewards):.2f}, Max: {max(total_rewards):.2f}")
                    agent.save_model()
                print("Testing with best model...")
                agent.collect_rollouts(ast_module, episodes=1, headless=False, max_steps=7200)
    except Exception as e:
        print(f"Error occurred: {e}")
    finally:
        pygame.quit()
