from pathlib import Path
from datetime import datetime
import calendar


def jd_to_datetime_utc(jd: float) -> datetime:
    """
    Convert a Julian Date (JD) to a UTC datetime.
    This is a standard JD -> Gregorian conversion; we ignore TT/TDB vs UTC
    differences, which is fine for an ephemeris header.
    """
    jd += 0.5
    Z = int(jd)
    F = jd - Z

    if Z < 2299161:
        A = Z
    else:
        alpha = int((Z - 1867216.25) / 36524.25)
        A = Z + 1 + alpha - int(alpha / 4)

    B = A + 1524
    C = int((B - 122.1) / 365.25)
    D = int(365.25 * C)
    E = int((B - D) / 30.6001)

    day = B - D - int(30.6001 * E) + F

    if E < 14:
        month = E - 1
    else:
        month = E - 13

    if month > 2:
        year = C - 4716
    else:
        year = C - 4715

    day_int = int(day)
    frac = day - day_int

    hours = frac * 24.0
    h = int(hours)
    minutes = (hours - h) * 60.0
    m = int(minutes)
    seconds = (minutes - m) * 60.0

    s = int(seconds)
    micro = int(round((seconds - s) * 1e6))

    # Handle overflow from rounding
    if micro == 1_000_000:
        micro = 0
        s += 1
    if s == 60:
        s = 0
        m += 1
    if m == 60:
        m = 0
        h += 1
    if h == 24:
        h = 0
        day_int += 1  # extremely rare edge case; good enough here

    return datetime(year, month, day_int, h, m, s, micro)


def format_scenario_epoch(dt: datetime) -> str:
    """
    Format datetime as STK-style ScenarioEpoch:
    'd Mmm yyyy hh:mm:ss.ssssss' (no leading zero on day).
    Example: '1 Jan 2031 17:00:00.000000'
    """
    day_str = str(dt.day)  # no leading zero
    month_str = calendar.month_abbr[dt.month]  # Jan, Feb, ...
    return f"{day_str} {month_str} {dt.year} {dt:%H:%M:%S.%f}"


def yyddd_from_datetime(dt: datetime) -> float:
    """
    Convert datetime to YYDDD.fraction format used in STK comments.
    Example: 2031-01-01 17:00 => 31001.708333...
    """
    year = dt.year
    start_of_year = datetime(year, 1, 1)
    delta = dt - start_of_year
    # Integer days since Jan 1 (0-based) + 1 for day-of-year
    day_of_year = delta.days + 1
    frac_day = (delta.seconds + delta.microseconds / 1e6) / 86400.0
    day_of_year_float = day_of_year + frac_day

    yy = year % 100
    return yy * 1000 + day_of_year_float


def convert_horizons(text: str) -> str:
    """
    Convert a JPL Horizons text ephemeris into an STK .e ephemeris file,
    matching the style of a working STK ephemeris:

    - NumberOfEphemerisPoints
    - ScenarioEpoch (calendar)
    - InterpolationMethod, InterpolationSamplesM1
    - Time column = seconds offset from ScenarioEpoch
    - CoordinateSystem (ICRF by default here)
    """
    lines = text.splitlines()
    in_block = False
    # We'll store (jd, [X, Y, Z, VX, VY, VZ])
    rows: list[tuple[float, list[str]]] = []

    for raw in lines:
        line = raw.strip()
        if not line:
            continue

        if line.startswith("$$SOE"):
            in_block = True
            continue
        if line.startswith("$$EOE"):
            in_block = False
            continue
        if not in_block:
            continue

        # Remove trailing comma if present
        if line.endswith(","):
            line = line[:-1]

        # Split on commas from Horizons
        fields = [f.strip() for f in line.split(",")]
        if not fields:
            continue

        # First field should be JDTDB
        try:
            jd_val = float(fields[0])
        except ValueError:
            continue

        if len(fields) < 8:
            # Need at least JDTDB + date + X Y Z VX VY VZ
            continue

        # fields layout:
        # 0: JDTDB
        # 1: Calendar Date (ignored)
        # 2: X
        # 3: Y
        # 4: Z
        # 5: VX
        # 6: VY
        # 7: VZ
        posvel = fields[2:8]  # X, Y, Z, VX, VY, VZ
        rows.append((jd_val, posvel))

    if not rows:
        raise ValueError("No ephemeris data rows found between $$SOE and $$EOE.")

    n_points = len(rows)
    first_jd = rows[0][0]
    epoch_dt = jd_to_datetime_utc(first_jd)
    scenario_epoch_str = format_scenario_epoch(epoch_dt)
    epoch_yyddd = yyddd_from_datetime(epoch_dt)

    # Build data lines: time in seconds from ScenarioEpoch
    data_rows = []
    seconds_per_day = 86400.0
    for jd_val, posvel in rows:
        t_seconds = (jd_val - first_jd) * seconds_per_day
        # Match STK style: scientific notation with 16 decimal places
        t_str = f"{t_seconds:.16e}"
        data_line = "  ".join([t_str] + posvel)
        data_rows.append(data_line)

    header_lines = [
        "stk.v.12.0",
        "",
        "# WrittenBy    Horizons_to_STK_converter",
        "",
        "BEGIN Ephemeris",
        "",
        f"    NumberOfEphemerisPoints\t\t {n_points}",
        "",
        f"    ScenarioEpoch\t\t {scenario_epoch_str}",
        "",
        f"# Epoch in JDate format: {first_jd:.14f}",
        f"# Epoch in YYDDD format:   {epoch_yyddd:.14f}",
        "",
        "    InterpolationMethod\t\t Lagrange",
        "",
        "    InterpolationSamplesM1\t\t 7",
        "",
        "    DistanceUnit\t\t Kilometers",
        "",
        "    CentralBody\t\t Sun",
        "",
        "    CoordinateSystem\t\t ICRF",
        "",
        f"# Time of first point: {scenario_epoch_str} UTCG = {first_jd:.14f} JDate = {epoch_yyddd:.14f} YYDDD",
        "",
        "    EphemerisTimePosVel\t\t",
        "",
    ]

    footer_line = "END Ephemeris"

    all_lines = header_lines + data_rows + ["", footer_line]
    return "\n".join(all_lines)


if __name__ == "__main__":
    script_dir = Path(__file__).parent
    name: str = "earth_ephem_2028"
    input_path = script_dir / "horizons_files" / f"{name}.txt"
    output_path = script_dir / "stk_files" / f"{name}.e"

    output_path.parent.mkdir(parents=True, exist_ok=True)

    data = input_path.read_text()
    result = convert_horizons(data)
    output_path.write_text(result)
