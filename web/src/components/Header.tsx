interface HeaderProps {
  all_ok: boolean;
  last_update?: string;
}

export default function Header({ all_ok, last_update }: HeaderProps) {
  return (
    <header className="bg-gradient-to-r from-gray-900 via-gray-800 to-gray-900 border-b border-gray-700/50 shadow-xl">
      <div className="container mx-auto px-3 sm:px-6 py-2 sm:py-4">
        <div className="flex items-center justify-between gap-2">
          {/* Logo and Title */}
          <div className="flex items-center gap-1.5 sm:gap-2">
            <div
              className="w-7 h-7 sm:w-10 sm:h-10 bg-contain bg-no-repeat bg-center flex-shrink-0"
              style={{
                backgroundImage: 'url(/logo.png)'
              }}
              aria-label="Quirky logo"
            />
            <h1 className="text-xl sm:text-3xl font-black text-white tracking-tight" style={{ fontFamily: "'Inter', 'Helvetica Neue', sans-serif" }}>
              quirky
            </h1>
          </div>

          {/* Status and Time - Compact on Mobile */}
          <div className="flex flex-col items-end gap-0.5 sm:flex-row sm:items-center sm:gap-3">
            {/* Status Badge */}
            <div className={`px-2 py-0.5 sm:px-3 sm:py-1 rounded-full text-[10px] sm:text-sm font-medium whitespace-nowrap ${
              all_ok
                ? 'bg-green-500/10 text-green-400 ring-1 ring-green-500/30'
                : 'bg-red-500/10 text-red-400 ring-1 ring-red-500/30'
            }`}>
              <span className="hidden sm:inline">{all_ok ? '✓ All Systems Operational' : '⚠ Issues Detected'}</span>
              <span className="sm:hidden">{all_ok ? '✓ OK' : '⚠'}</span>
            </div>

            {/* Last Updated */}
            {last_update && (
              <div className="text-[10px] sm:text-sm text-gray-400 font-medium whitespace-nowrap">
                <span className="text-gray-300">{new Date(last_update).toLocaleString(undefined, {
                  month: 'short',
                  day: 'numeric',
                  hour: '2-digit',
                  minute: '2-digit'
                })}</span>
              </div>
            )}
          </div>
        </div>
      </div>
    </header>
  );
}
