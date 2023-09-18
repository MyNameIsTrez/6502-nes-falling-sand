def main():
    """
    The order of the 8 bits per tile is TL-TR-BL-BR,
    where T stands for top, and L for left.

    A tile consists of two bit planes, where the first is the low bit:
    https://www.nesdev.org/wiki/PPU_pattern_tables

    00 is air, 01 is stone, 10 is sand, 11 is water.

    0b11110000 is setting the left cell of a row on.
    """
    for tile_index in range(256):
        bitplane_1_br = bool(tile_index & 0b00000001)
        bitplane_1_bl = bool(tile_index & 0b00000100)
        bitplane_1_tr = bool(tile_index & 0b00010000)
        bitplane_1_tl = bool(tile_index & 0b01000000)

        bitplane_2_br = bool(tile_index & 0b00000010)
        bitplane_2_bl = bool(tile_index & 0b00001000)
        bitplane_2_tr = bool(tile_index & 0b00100000)
        bitplane_2_tl = bool(tile_index & 0b10000000)

        bitplane_1_values = []
        for _cell_row in range(4):
            row_value = 0
            row_value |= bitplane_1_tr * 0b00001111
            row_value |= bitplane_1_tl * 0b11110000
            bitplane_1_values.append(hx(row_value))

        for _cell_row in range(4):
            row_value = 0
            row_value |= bitplane_1_br * 0b00001111
            row_value |= bitplane_1_bl * 0b11110000
            bitplane_1_values.append(hx(row_value))

        bitplane_2_values = []
        for _cell_row in range(4):
            row_value = 0
            row_value |= bitplane_2_tr * 0b00001111
            row_value |= bitplane_2_tl * 0b11110000
            bitplane_2_values.append(hx(row_value))

        for _cell_row in range(4):
            row_value = 0
            row_value |= bitplane_2_br * 0b00001111
            row_value |= bitplane_2_bl * 0b11110000
            bitplane_2_values.append(hx(row_value))

        print(
            "\t; "
            + material(bitplane_1_tl | (bitplane_2_tl << 1))
            + ","
            + material(bitplane_1_tr | (bitplane_2_tr << 1))
            + ","
            + material(bitplane_1_bl | (bitplane_2_bl << 1))
            + ","
            + material(bitplane_1_br | (bitplane_2_br << 1))
            + f" at index {hx(tile_index)}"
        )

        print("\t.byte " + ",".join(bitplane_1_values) + " ; Low bit plane")

        print("\t.byte " + ",".join(bitplane_2_values) + " ; High bit plane")

        print("")


def hx(n):
    return hex(n).replace("0x", "$")


def material(n):
    if n == 0:
        return "air"
    if n == 1:
        return "stone"
    if n == 2:
        return "sand"
    return "water"


if __name__ == "__main__":
    main()
