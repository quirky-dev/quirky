import { HealthResult, HealthStatus } from '../types';

interface CheckItemProps {
  name: string;
  result: HealthResult;
}

const statusConfig: Record<HealthStatus, { icon: string; color: string; bg: string; ring: string }> = {
  ok: {
    icon: '✓',
    color: 'text-green-400',
    bg: 'bg-green-500/5',
    ring: 'ring-1 ring-green-500/20',
  },
  warning: {
    icon: '⚠',
    color: 'text-yellow-400',
    bg: 'bg-yellow-500/5',
    ring: 'ring-1 ring-yellow-500/20',
  },
  error: {
    icon: '✕',
    color: 'text-red-400',
    bg: 'bg-red-500/5',
    ring: 'ring-1 ring-red-500/20',
  },
};

export default function CheckItem({ name, result }: CheckItemProps) {
  const config = statusConfig[result.status];

  return (
    <div className={`rounded-lg p-3 ${config.bg} ${config.ring} transition-colors hover:bg-opacity-80`}>
      <div className="flex items-center justify-between gap-2">
        <div className="flex items-center gap-2 flex-1 min-w-0">
          <span className={`text-base font-bold ${config.color} flex-shrink-0`}>{config.icon}</span>
          <h4 className="font-medium text-white text-sm truncate">{name}</h4>
        </div>
        <span className={`text-[10px] font-bold uppercase px-1.5 py-0.5 rounded ${config.color} ${config.bg} ${config.ring} flex-shrink-0`}>
          {result.status}
        </span>
      </div>

      <p className="text-xs text-gray-400 mt-1.5 ml-6">{result.message}</p>

      {result.data && Object.keys(result.data).length > 0 && (
        <details className="mt-2 ml-6">
          <summary className="text-xs text-gray-500 cursor-pointer hover:text-gray-400 select-none">
            details →
          </summary>
          <pre className="mt-1.5 text-[10px] bg-black/30 p-2 rounded overflow-x-auto text-gray-500 leading-relaxed">
            {JSON.stringify(result.data, null, 2)}
          </pre>
        </details>
      )}
    </div>
  );
}
