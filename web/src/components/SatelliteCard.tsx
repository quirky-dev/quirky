import { SatelliteStatus } from '../types';
import CheckItem from './CheckItem';

interface SatelliteCardProps {
  name: string;
  satellite: SatelliteStatus;
}

export default function SatelliteCard({ name, satellite }: SatelliteCardProps) {
  const checks = Object.entries(satellite.checks);
  const isError = satellite.status === 'error';

  return (
    <div className={`bg-gradient-to-br from-gray-800 to-gray-900 border rounded-xl overflow-hidden shadow-xl transition-all hover:shadow-2xl ${
      isError ? 'border-red-500/30 hover:border-red-500/50' : 'border-gray-700/50 hover:border-gray-600/50'
    }`}>
      <div className={`px-4 py-3 border-b backdrop-blur-sm ${
        isError ? 'bg-red-900/10 border-red-500/20' : 'bg-white/5 border-gray-700/30'
      }`}>
        <div className="flex items-center justify-between">
          <h3 className="text-lg font-bold text-white tracking-tight">{name}</h3>
          <div className={`px-2.5 py-0.5 rounded-full text-xs font-bold uppercase tracking-wider ${
            isError
              ? 'bg-red-500/10 text-red-400 ring-1 ring-red-500/30'
              : 'bg-green-500/10 text-green-400 ring-1 ring-green-500/30'
          }`}>
            {isError ? '✗ Error' : '✓ OK'}
          </div>
        </div>
      </div>

      <div className="p-4">
        {satellite.error ? (
          <div className="text-red-400 bg-red-900/10 border border-red-500/30 rounded-lg px-3 py-2.5">
            <p className="font-semibold text-sm mb-1">Connection Error</p>
            <p className="text-xs opacity-90">{satellite.error}</p>
          </div>
        ) : (
          <div className="space-y-2">
            {checks.length === 0 ? (
              <p className="text-gray-400 text-sm">No checks configured</p>
            ) : (
              checks.map(([checkName, result]) => (
                <CheckItem key={checkName} name={checkName} result={result} />
              ))
            )}
          </div>
        )}
      </div>
    </div>
  );
}
