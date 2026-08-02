import { defineConfig } from 'vitepress'

export default defineConfig({
  title: '星枢 Xisp',
  description: '仓颉嵌入式 Lisp 脚本语言',
  lang: 'zh-CN',
  base: '/xisp/',
  outDir: '../docs/dist',
  lastUpdated: true,
  themeConfig: {
    nav: [
      { text: '指南', link: '/guide/01-quickstart' },
      { text: 'API 参考', link: '/api/' },
      { text: '集成', link: '/integration/embedding' },
    ],
    sidebar: {
      '/guide/': [
        {
          text: '入门',
          items: [
            { text: '快速开始', link: '/guide/01-quickstart' },
            { text: '基础语法', link: '/guide/02-basics' },
            { text: '现代语法', link: '/guide/03-modern' },
          ],
        },
        {
          text: '进阶',
          items: [
            { text: '宏系统', link: '/guide/04-macros' },
            { text: '模块系统', link: '/guide/05-modules' },
            { text: 'Unicode 支持', link: '/guide/06-unicode' },
            { text: 'REPL 指南', link: '/guide/07-repl' },
          ],
        },
      ],
      '/api/': [
        {
          text: '内置函数',
          items: [
            { text: '概览', link: '/api/index' },
            { text: '算术与比较', link: '/api/arithmetic' },
            { text: '列表操作', link: '/api/list' },
            { text: '哈希映射', link: '/api/hashmap' },
            { text: '谓词', link: '/api/predicates' },
            { text: '逻辑', link: '/api/logic' },
            { text: '字符串', link: '/api/string' },
            { text: '流程控制', link: '/api/control' },
          ],
        },
      ],
      '/integration/': [
        {
          text: '集成',
          items: [
            { text: '嵌入解释器', link: '/integration/embedding' },
            { text: '选项系统', link: '/integration/options' },
            { text: '沙箱', link: '/integration/sandbox' },
            { text: '桥接', link: '/integration/bridge' },
          ],
        },
      ],
    },
    socialLinks: [
      { icon: 'github', link: 'https://github.com/ystyle/xisp' },
    ],
    footer: {
      message: 'MIT License',
    },
    search: {
      provider: 'local',
    },
  },
})
