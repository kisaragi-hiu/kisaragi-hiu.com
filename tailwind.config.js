let cjkFallback = [
  "Noto Sans CJK TC",
  "Microsoft Jhenghei",
  "Microsoft Yahei",
  "Meiryo",
  "Malgun Gothic",
];

module.exports = {
  purge: {
    content: ["./templates/**/*.html"],
    options: { safeList: { deep: [/highlight$/] } },
  },
  mode: "jit",
  darkMode: false, // or 'media' or 'class'
  theme: {
    fontFamily: {
      sans: ["Inter", ...cjkFallback, "sans-serif"],
      // serif: ["Equity"],
      mono: [
        "Overpass Mono",
        "Noto Sans Mono CJK",
        ...cjkFallback,
        "monospace",
      ],
    },
    extend: {
      // https://github.com/tailwindlabs/tailwindcss/discussions/1361
      colors: {
        primary: "#0d0d0d",
        secondary: "#333333",
        special: "#cdecff",
        "special-strong": "#246084",
        "special-light": "#cdecff50",
      },
    },
  },
  variants: {
    extend: {},
  },
  plugins: [],
};
