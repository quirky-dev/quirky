# Quirky Web Dashboard

Modern React-based monitoring dashboard for Quirky.

## Tech Stack

- **React 19** - UI framework
- **TypeScript** - Type safety
- **RSBuild** - Fast build tool
- **Tailwind CSS v4** - Utility-first styling

## Development

```bash
# Install dependencies
npm install

# Start development server
npm run dev

# Build for production
npm run build

# Preview production build
npm run preview
```

## Configuration

The dashboard connects to the Quirky aggregator API via proxy. By default:

- Frontend dev server: `http://localhost:3000`
- API proxy: `/api` → `http://localhost:8081`

To change the API endpoint, edit `rsbuild.config.ts`:

```typescript
server: {
  proxy: {
    '/api': {
      target: 'http://your-aggregator:8081',
      changeOrigin: true,
    },
  },
}
```

## Features

- **Real-time monitoring** - Auto-refreshes every 30 seconds
- **Satellite overview** - View all monitored satellites at a glance
- **Health check details** - Drill down into individual check results
- **Status indicators** - Clear visual status (OK, Warning, Error)
- **Dark theme** - Easy on the eyes for monitoring dashboards
- **Responsive design** - Works on desktop and mobile

## Building for Production

```bash
npm run build
```

The built files will be in the `dist/` directory. Serve these static files with any web server (nginx, Apache, etc.) or the built-in Haskell server.
