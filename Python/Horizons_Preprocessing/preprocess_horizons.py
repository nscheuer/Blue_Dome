from pathlib import Path


def convert_horizons(text: str) -> str:
    """
    Convert a JPL Horizons text ephemeris into an STK .e ephemeris file.

    Steps:
    - Extract data lines between $$SOE and $$EOE
    - Drop the calendar-date column
    - Keep only: JDTDB, X, Y, Z, VX, VY, VZ
      (i.e. remove LT, RG, RR)
    - Remove commas and format with double spaces between columns
    - Wrap with the STK Ephemeris header/footer
    """
    lines = text.splitlines()
    in_block = False
    data_rows = []

    for raw in lines:
        line = raw.strip()
        if not line:
            continue

        # Switch on/off when we hit $$SOE / $$EOE
        if line.startswith("$$SOE"):
            in_block = True
            continue
        if line.startswith("$$EOE"):
            in_block = False
            continue
        if not in_block:
            continue

        # Trim trailing comma from data lines if present
        if line.endswith(","):
            line = line[:-1]

        # Split CSV-style on commas
        fields = [f.strip() for f in line.split(",")]

        if not fields:
            continue

        # First field should be JDTDB (numeric) – skip anything else
        try:
            float(fields[0])
        except ValueError:
            continue

        if len(fields) < 8:
            # Not enough numeric fields; skip
            continue

        # Horizons layout for these lines:
        # 0: JDTDB
        # 1: Calendar Date (string, we drop this)
        # 2: X
        # 3: Y
        # 4: Z
        # 5: VX
        # 6: VY
        # 7: VZ
        # 8: LT
        # 9: RG
        # 10: RR
        #
        # We want: JDTDB + X,Y,Z,VX,VY,VZ (drop LT, RG, RR)
        selected = [fields[0]] + fields[2:8]

        # Join with exactly two spaces between entries
        data_line = "  ".join(selected)
        data_rows.append(data_line)

    # STK header/footer
    header_lines = [
        "stk.v.12.0",
        "BEGIN Ephemeris",
        "    InterpolationMethod     Lagrange",
        "    InterpolationOrder      5",
        "    DistanceUnit            Kilometers",
        "    CentralBody             Sun",
        "    CoordinateSystem        EclipticJ2000",
        "    TimeFormat              JDate",
        "    EphemerisTimePosVel",
    ]
    footer_line = "END Ephemeris"

    all_lines = header_lines + data_rows + [footer_line]
    return "\n".join(all_lines)


if __name__ == "__main__":
    script_dir = Path(__file__).parent
    input_path = script_dir / "horizons_files" / "2011_ah4_ephem.txt"
    output_path = script_dir / "stk_files" / "2011_ah4_ephem.e"

    # Make sure the output directory exists
    output_path.parent.mkdir(parents=True, exist_ok=True)

    data = input_path.read_text()
    result = convert_horizons(data)
    output_path.write_text(result)
