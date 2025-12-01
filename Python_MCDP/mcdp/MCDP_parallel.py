import numpy as np
import matplotlib.pyplot as plt
from typing import List, Tuple, Dict, Any
from itertools import product
from concurrent.futures import ProcessPoolExecutor, as_completed
import os

# Assuming these imports exist in your project structure
from hazard.hazard import HazardFunction
from neo_models.size_distribution import SizeDistribution
from neo_models.neo_distribution import NEODistribution
from sensors.ground_sensor import GroundSensor
from sensors.space_sensor import SpaceSensor
from sensors.combined_sensor import CombinedSensor

# --- Helper Function for Parallelization ---
# This must be a top-level function or a static method to be picklable by multiprocessing.
def _worker_evaluate_config(args: Tuple) -> Dict[str, Any]:
    """
    Worker function to evaluate a single configuration in a separate process.
    """
    (ground_counts, space_counts, 
     ground_sensors, space_sensors, 
     size_dist, vel_dist, pv_dist, 
     tau_max_days, n_mc, target, 
     ground_costs, space_costs) = args

    # 1. Calculate Cost (Vectorized / Fast)
    # We use the pre-computed cost arrays passed in args
    cost = np.dot(ground_counts, ground_costs) + np.dot(space_counts, space_costs)

    # 2. Build Combined Sensor
    # We must reconstruct this in the worker process
    ground_list = []
    for sensor, n in zip(ground_sensors, ground_counts):
        if n > 0:
            ground_list.extend([sensor] * n)

    space_list = []
    for sensor, n in zip(space_sensors, space_counts):
        if n > 0:
            space_list.extend([sensor] * n)

    combined = CombinedSensor(ground_sensors=ground_list, space_sensors=space_list)

    # 3. Run Hazard Simulation
    # Note: HazardFunction runs compute_tti_distribution in __init__ automatically
    hazard = HazardFunction(
        combined_sensor=combined,
        size_dist=size_dist,
        vel_dist=vel_dist,
        pv_dist=pv_dist,
        tau_max_days=tau_max_days,
        n_mc=n_mc,
    )

    expected_tau = hazard.time_to_detect_percent(target=target)

    return {
        "tau": expected_tau,
        "cost": cost,
        "ground_counts": ground_counts,
        "space_counts": space_counts,
    }


class MCDP_Parallel:
    """
    Optimized Multi-Criteria Design Problem for NEA sensor architecture.
    Features:
      - Multiprocessing for hazard simulations.
      - O(N log N) Pareto sorting.
    """
    def __init__(
        self,
        ground_sensors: List[GroundSensor],
        space_sensors: List[SpaceSensor],
        ground_max_counts: List[int],
        space_max_counts: List[int],
        size_dist: SizeDistribution,
        vel_dist: NEODistribution,
        pv_dist: NEODistribution,
        target: float = 0.03,
        n_mc: float = 100
    ) -> None:

        if len(ground_sensors) != len(ground_max_counts):
            raise ValueError("ground_sensors and ground_max_counts must have same length.")
        if len(space_sensors) != len(space_max_counts):
            raise ValueError("space_sensors and space_max_counts must have same length.")

        self.ground_sensors = ground_sensors
        self.space_sensors = space_sensors
        self.ground_max_counts = ground_max_counts
        self.space_max_counts = space_max_counts

        # Pre-extract costs for fast vector calculation later
        self.ground_costs = np.array([s.fixedcost for s in ground_sensors])
        self.space_costs = np.array([s.fixedcost for s in space_sensors])

        self.size_dist = size_dist
        self.vel_dist = vel_dist
        self.pv_dist = pv_dist

        self.tau_max_days = 365.0*5
        self.n_mc = n_mc
        self.target = target

        self.results: List[dict] = []
        self.pareto_indices: np.ndarray | None = None

    # ---------------------------------------------------------
    @staticmethod
    def _pareto_front_optimized(results: List[dict]) -> np.ndarray:
        """
        O(N log N) Pareto frontier calculation.
        
        Logic:
        1. Sort by Cost (Ascending).
        2. Iterate: A point is Pareto optimal if its 'tau' is higher 
           than the running maximum 'tau' found so far among cheaper options.
        """
        # Convert to structured array or just simple lists for sorting
        n = len(results)
        costs = np.array([r["cost"] for r in results])
        taus = np.array([r["tau"] for r in results])
        original_indices = np.arange(n)

        # 1. Sort by Cost (primary) and Tau (secondary, descending)
        # We use lexsort. Note: lexsort sorts by the last key passed first.
        # We want Cost Ascending.
        sorted_order = np.lexsort(( -taus, costs )) # Sort by cost asc, then tau desc
        
        pareto_indices = []
        max_tau_so_far = -1.0

        for i in sorted_order:
            current_tau = taus[i]
            
            # If this configuration provides better warning than all cheaper options
            # (or equal warning for cheaper cost due to sorting order), keep it.
            if current_tau > max_tau_so_far:
                pareto_indices.append(i)
                max_tau_so_far = current_tau
                
        return np.array(pareto_indices)

    # ---------------------------------------------------------
    def run(self, max_workers: int = None):
        """
        Execute the parameter sweep in parallel.
        
        Parameters
        ----------
        max_workers : int, optional
            Number of CPU cores to use. Defaults to os.cpu_count().
        """
        self.results = []
        if max_workers is None:
            max_workers = os.cpu_count() or 1

        print(f"Starting MCDP simulation on {max_workers} cores...")

        # 1. Generate all combinations
        ground_ranges = [range(m + 1) for m in self.ground_max_counts]
        space_ranges = [range(m + 1) for m in self.space_max_counts]
        
        tasks = []
        
        # 2. Prepare arguments for workers
        # We flatten the inputs into a tuple to pass to the static worker method
        for g_counts in product(*ground_ranges):
            for s_counts in product(*space_ranges):
                
                if sum(g_counts) + sum(s_counts) == 0:
                    continue

                task_args = (
                    g_counts, 
                    s_counts,
                    self.ground_sensors, 
                    self.space_sensors,
                    self.size_dist, 
                    self.vel_dist, 
                    self.pv_dist,
                    self.tau_max_days, 
                    self.n_mc, 
                    self.target,
                    self.ground_costs,
                    self.space_costs
                )
                tasks.append(task_args)

        if not tasks:
            raise RuntimeError("No configurations to evaluate.")

        # 3. Parallel Execution
        # We use a ProcessPoolExecutor to run simulations in parallel
        with ProcessPoolExecutor(max_workers=max_workers) as executor:
            # We submit all tasks
            future_to_args = {executor.submit(_worker_evaluate_config, args): args for args in tasks}
            
            # As they complete, we collect results
            count = 0
            total = len(tasks)
            
            # Optional: Use tqdm if available, otherwise simple print
            try:
                from tqdm import tqdm
                iterator = tqdm(as_completed(future_to_args), total=total, desc="Simulating")
            except ImportError:
                iterator = as_completed(future_to_args)

            for future in iterator:
                try:
                    res = future.result()
                    self.results.append(res)
                except Exception as exc:
                    print(f"Task generated an exception: {exc}")
                
                # Simple progress fallback if tqdm missing
                if 'tqdm' not in locals():
                    count += 1
                    if count % 10 == 0:
                        print(f"Processed {count}/{total} configurations...", end='\r')

        print(f"\nSimulation complete. Evaluated {len(self.results)} configurations.")
        
        # 4. Calculate Pareto Front (Optimized)
        self.pareto_indices = self._pareto_front_optimized(self.results)
        print(f"Found {len(self.pareto_indices)} Pareto-optimal architectures.")

    # ---------------------------------------------------------
    # PLOTTING (Identical to original)
    # ---------------------------------------------------------
    def plot_pareto(self):
        """
        Interactive Pareto plot with detailed inspection.
        
        Left: Cost vs Warning Time Scatter.
        Top Right: Text details of selected config.
        Bottom Right: Survival Curve of selected config (computed on the fly).
        """
        if not self.results:
            raise RuntimeError("No results. Run MCDP.run() first.")

        if self.pareto_indices is None:
            self.pareto_indices = self._pareto_front_optimized(self.results)

        costs = np.array([r["cost"] for r in self.results])
        taus = np.array([r["tau"] for r in self.results])

        # 1. Setup Custom Layout (GridSpec)
        # Left column (scatter) takes width 3, Right column (info/plot) takes width 2
        fig = plt.figure(figsize=(14, 7))
        gs = fig.add_gridspec(2, 2, width_ratios=[1.5, 1], height_ratios=[1, 1])

        ax_scatter = fig.add_subplot(gs[:, 0]) # Left: Spans both rows
        ax_text    = fig.add_subplot(gs[0, 1]) # Right Top: Text
        ax_surv    = fig.add_subplot(gs[1, 1]) # Right Bottom: Survival Plot

        # 2. Plot Scatter (Pareto & All)
        # All points (pickable)
        scat_all = ax_scatter.scatter(
            costs, taus,
            alpha=0.25, color="steelblue",
            picker=True, s=30, label="All configs"
        )
        
        # Pareto points (highlighted)
        pareto_idx = np.array(self.pareto_indices)
        ax_scatter.scatter(
            costs[pareto_idx], taus[pareto_idx],
            color="red", s=50, label="Pareto front"
        )

        ax_scatter.set_xlabel("Total Fixed Cost ($)")
        ax_scatter.set_ylabel("Expected Warning Time E[τ] (days)")
        ax_scatter.set_title("Pareto Front: Early Detection vs Cost")
        ax_scatter.grid(True, alpha=0.3)
        ax_scatter.legend()

        # 3. Setup Detail Panels (Initial State)
        ax_text.axis("off")
        ax_text.set_title("Configuration Details")
        
        ax_surv.set_xlabel("Time To Impact (days)")
        ax_surv.set_ylabel("Survival Probability")
        ax_surv.set_title("Survival Function (Click a point)")
        ax_surv.grid(True, alpha=0.3)
        ax_surv.set_ylim(0, 1.05)

        # Store reference to the highlighted point to remove it later
        state = {"highlight_artist": None}

        # --- Helper: Re-run simulation for one point ---
        def get_survival_curve(idx):
            """Reconstructs sensor and runs hazard function for ONE config."""
            r = self.results[idx]
            
            # Rebuild Combined Sensor
            ground_list = []
            for sensor, n in zip(self.ground_sensors, r["ground_counts"]):
                if n > 0: ground_list.extend([sensor] * n)

            space_list = []
            for sensor, n in zip(self.space_sensors, r["space_counts"]):
                if n > 0: space_list.extend([sensor] * n)

            combined = CombinedSensor(ground_sensors=ground_list, space_sensors=space_list)

            # Re-run Hazard Logic
            # Note: This is fast for a single run (fraction of a second)
            haz = HazardFunction(
                combined_sensor=combined,
                size_dist=self.size_dist,
                vel_dist=self.vel_dist,
                pv_dist=self.pv_dist,
                tau_max_days=self.tau_max_days,
                n_mc=self.n_mc 
            )
            
            return haz.tau_grid, haz.survival

        # --- Update View Function ---
        def update_view(idx):
            r = self.results[idx]
            
            # A. Update Text
            lines = []
            lines.append(f"Index: {idx}")
            lines.append(f"E[τ_det]: {r['tau']:.2f} days")
            lines.append(f"Cost: ${r['cost']/1e6:.1f} M")
            lines.append("-" * 20)
            lines.append("Ground:")
            for sensor, n in zip(self.ground_sensors, r["ground_counts"]):
                if n > 0: lines.append(f"  {sensor.name}: {n}")
            lines.append("Space:")
            for sensor, n in zip(self.space_sensors, r["space_counts"]):
                if n > 0: lines.append(f"  {sensor.name}: {n}")

            ax_text.clear()
            ax_text.axis("off")
            ax_text.text(0.05, 0.95, "\n".join(lines), va="top", ha="left", fontsize=10, family='monospace')
            ax_text.set_title("Configuration Details")

            # B. Update Survival Plot
            tau_grid, survival = get_survival_curve(idx)
            
            ax_surv.clear()
            ax_surv.plot(tau_grid, survival, lw=2, color='darkgreen', label='Survival S(τ)')
            
            # Add Target Line
            ax_surv.axhline(self.target, color='red', linestyle='--', alpha=0.7, label=f'Target ({self.target})')
            
            # Add Marker for the detected time
            # We can visually show where the curve crosses the target
            cross_idx = np.argmax(survival > self.target)
            if cross_idx > 0 and cross_idx < len(tau_grid):
                cross_tau = tau_grid[cross_idx]
                ax_surv.plot(cross_tau, survival[cross_idx], 'ro')
                ax_surv.annotate(f"{cross_tau:.1f}d", (cross_tau, survival[cross_idx]), 
                                 xytext=(5, 5), textcoords='offset points', color='red')

            ax_surv.set_xlabel("Time To Impact (days)")
            ax_surv.set_ylabel("Survival Probability")
            ax_surv.set_title("Risk Profile (Survival Function)")
            ax_surv.grid(True, alpha=0.3)
            ax_surv.legend(loc='upper left')
            ax_surv.set_ylim(-0.02, 1.02)

            # C. Highlight Point on Scatter
            if state["highlight_artist"]:
                state["highlight_artist"].remove()
            
            state["highlight_artist"] = ax_scatter.scatter(
                [costs[idx]], [taus[idx]],
                s=200, facecolors="none", edgecolors="black", linewidths=2, zorder=10
            )

            fig.canvas.draw_idle()

        # 4. Connect Events
        def on_pick(event):
            if event.artist != scat_all:
                return
            idx = event.ind[0]
            # Change cursor to busy? (Optional)
            update_view(idx)

        fig.canvas.mpl_connect("pick_event", on_pick)
        plt.tight_layout()